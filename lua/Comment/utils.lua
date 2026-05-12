---@mod comment.utils Utilities

local Ft = require('Comment.ft')

local U = {}

---Comment context
---@class CommentCtx
---@field ctype integer See |comment.utils.ctype|
---@field cmode integer See |comment.utils.cmode|
---@field cmotion integer See |comment.utils.cmotion|
---@field range CommentRange

---Range of the selection that needs to be commented
---@class CommentRange
---@field srow integer Starting row (1-based)
---@field scol integer Starting column (1-based)
---@field erow integer Ending row (1-based, inclusive)
---@field ecol integer Ending column (1-based, inclusive)

---Comment modes - Can be manual or computed via operator-mode
---@class CommentMode
---@field toggle integer Toggle action
---@field comment integer Comment action
---@field uncomment integer Uncomment action

---An object containing comment modes
---@type CommentMode
U.cmode = {
    toggle = 0,
    comment = 1,
    uncomment = 2,
}

---Comment types
---@class CommentType
---@field linewise integer Use linewise commentstring
---@field blockwise integer Use blockwise commentstring

---An object containing comment types
---@type CommentType
U.ctype = {
    linewise = 1,
    blockwise = 2,
}

---Comment motion types
---@class CommentMotion
---@field line integer Line motion (ie. 'gc2j')
---@field char integer Character/left-right motion (ie. 'gc2w')
---@field block integer Visual operator-pending motion
---@field v integer Visual motion (ie. 'v3jgc')
---@field V integer Visual-line motion (ie. 'V10kgc')

---An object containing comment motions
---@type CommentMotion
U.cmotion = {
    line = 1,
    char = 2,
    block = 3,
    v = 4,
    V = 5,
}

---@private
---Check whether the line is empty
---@param iter string|string[]
---@return boolean
function U.is_empty(iter)
    return #iter == 0
end

---@private
---Helper to get padding character
---@param flag boolean
---@return string string
function U.get_pad(flag)
    return flag and ' ' or ''
end

---@private
---Helper to get padding pattern
---@param flag boolean
---@return string string
function U.get_padpat(flag)
    return flag and '%s?' or ''
end

---@private
---Call a function if exists
---@param fn unknown|fun(...):unknown Wanna be function
---@return unknown
function U.is_fn(fn, ...)
    if type(fn) == 'function' then
        return fn(...)
    end
    return fn
end

---@private
---Check if the given line is ignored or not with the given pattern
---@param ln string Line to be ignored
---@param pat string Lua regex
---@return boolean
function U.ignore(ln, pat)
    return pat and string.find(ln, pat) ~= nil
end

---Get region for line movement or visual selection
---NOTE: Returns the current line region, if `opmode` is not given.
---@param opmode? OpMotion
---@return CommentRange
function U.get_region(opmode)
    if not opmode then
        local row = unpack(vim.api.nvim_win_get_cursor(0))
        return { srow = row, scol = 0, erow = row, ecol = 0 }
    end

    -- local marks = opmode:match('[vV]') and { '<', '>' } or { '[', ']' }
    if opmode:match('[vV]') then
        local srow, erow = vim.fn.line('v'), vim.fn.line('.')
        local scol, ecol = vim.fn.col('v'), vim.fn.col('.')
        if srow > erow or (srow == erow and scol > ecol) then
            srow, erow, scol, ecol = erow, srow, ecol, scol
        end
        return { srow = srow, scol = scol, erow = erow, ecol = ecol }
    end

    -- local sln, eln = vim.api.nvim_buf_get_mark(0, marks[1]), vim.api.nvim_buf_get_mark(0, marks[2])
    local sln, eln = vim.api.nvim_buf_get_mark(0, '['), vim.api.nvim_buf_get_mark(0, ']')

    return { srow = sln[1], scol = sln[2]+1, erow = eln[1], ecol = eln[2]+1 }
end

---Get lines from the current position to the given count
---@param count integer Probably 'vim.v.count'
---@return string[] #List of lines
---@return CommentRange
function U.get_count_lines(count)
    local srow = unpack(vim.api.nvim_win_get_cursor(0))
    local erow = (srow + count) - 1
    local lines = vim.api.nvim_buf_get_lines(0, srow - 1, erow, false)

    return lines, { srow = srow, scol = 0, erow = erow, ecol = 0 }
end

---Get lines from a NORMAL/VISUAL mode
---@param range CommentRange
---@return string[] #List of lines
function U.get_lines(range)
    -- If start and end is same, then just return the current line
    if range.srow == range.erow then
        return { vim.api.nvim_get_current_line() }
    end

    return vim.api.nvim_buf_get_lines(0, range.srow - 1, range.erow, false)
end

---Validates and unwraps the given commentstring
---@param cstr string See 'commentstring'
---@return string string Left side of the commentstring
---@return string string Right side of the commentstring
function U.unwrap_cstr(cstr)
    local left, right = string.match(cstr, '(.*)%%s(.*)')

    assert(
        (left or right),
        { msg = string.format('Invalid commentstring for %s! Read `:h commentstring` for help.', vim.bo.filetype) }
    )

    return vim.trim(left), vim.trim(right)
end

---Parses commentstring from the following places in the respective order
---  1. pre_hook - commentstring returned from the function
---  2. ft.lua - commentstring table bundled with the plugin
---  3. commentstring - Neovim's native. See 'commentstring'
---@param cfg CommentConfig
---@param ctx CommentCtx
---@return string string Left side of the commentstring
---@return string string Right side of the commentstring
function U.parse_cstr(cfg, ctx)
    -- 1. We ask `pre_hook` for a commentstring
    local inbuilt = U.is_fn(cfg.pre_hook, ctx)
        -- 2. Calculate w/ the help of treesitter
        or Ft.calculate(ctx)

    assert(inbuilt or (ctx.ctype ~= U.ctype.blockwise), {
        msg = vim.bo.filetype .. " doesn't support block comments!",
    })

    -- 3. Last resort to use native commentstring
    return U.unwrap_cstr(inbuilt or vim.bo.commentstring)
end

---Returns a closure which is used to do comments
---
---If given {string[]} to the closure then it will do blockwise comment
---else linewise comment will be done with the given {string}
---@param left string Left side of the commentstring
---@param right string Right side of the commentstring
---@param padding boolean Is padding enabled?
---@param scol? integer Starting column
---@param ecol? integer Ending column
---@param tabbed? boolean Using tab indentation
---@return (fun(line: string) : string) | (fun(input: string[]) :string[])
function U.commenter(left, right, padding, scol, ecol, tabbed)
    local pad = U.get_pad(padding)
    local ll = U.is_empty(left) and left or (left .. pad)
    local rr = U.is_empty(right) and right or (pad .. right)
    local empty = string.rep(tabbed and '\t' or ' ', scol or 0) .. ll .. rr
    return
    ---@param input string | string[] Input to be commented; can be a string or a list of strings. Will be mutated!
    ---@return string | string[]
        function(input)
            if type(input) == "string" then
                ------------------
                -- for linewise --
                ------------------
                if U.is_empty(input) then
                    return empty
                end
                -- line == 0 -> start from 0 col
                if scol == 1 then
                    return (ll .. input .. rr)
                end
                local first = input:sub(1, scol - 1)
                local last = input:sub(scol, -1)
                return first .. ll .. last .. rr
            end
            -------------------
            -- for blockwise --
            -------------------
            local n = #input
            -- If both columns are given then we can assume it's a partial block
            if scol and ecol then
                input[1] = input[1]:sub(1, scol - 1) .. ll .. input[1]:sub(scol, -1)
                if n == 1 then ecol = ecol + #ll end
                input[n] = input[n]:sub(1, ecol) .. rr .. input[n]:sub(ecol + 1, -1)
            else
                input[1] = U.is_empty(input[1]) and left or input[1]:gsub('^(%s*)', '%1' .. vim.pesc(ll))
                input[n] = U.is_empty(input[n]) and right or (input[n] .. rr)
            end
            return input
        end
end

---Returns a closure which is used to uncomment a line
---
---If given {string[]} to the closure then it will block uncomment
---else linewise uncomment will be done with the given {string}
---@param l_cstr string Left side of the commentstring
---@param r_cstr string Right side of the commentstring
---@param padding boolean Is padding enabled?
---@param scol? integer Starting column
---@param ecol? integer Ending column
---@return (fun(line: string) : string) | (fun(input: string|string[]) : string|string[])
function U.uncommenter(l_cstr, r_cstr, padding, scol, ecol)
    local padpattern, padlen = U.get_padpat(padding), padding and 1 or 0
    local left_len, right_len = #l_cstr + padlen, #r_cstr + padlen
    local lpattern = U.is_empty(l_cstr) and l_cstr or vim.pesc(l_cstr) .. padpattern
    local rpattern = U.is_empty(r_cstr) and r_cstr or padpattern .. vim.pesc(r_cstr)
    local pattern = '^(%s*)' .. lpattern .. '(.-)' .. rpattern .. '$'
    return
    ---@param input string | string[] Input to be uncommented; can be a string or a list of strings. Will be mutated!
    ---@return string | string[]
        function(input)
            if type(input) == 'string' then
                ------------------
                -- for linewise --
                ------------------
                local a, b = input:match(pattern)
                -- When user tries to uncomment when there is nothing to uncomment.
                -- See https://github.com/numToStr/Comment.nvim/issues/221
                assert(a and b, { msg = 'Nothing to uncomment!' })
                -- If there is nothing after LHS then just return ''
                -- bcz the line previously (before comment) was empty
                return a .. b
            end
            -------------------
            -- for blockwise --
            -------------------
            local n = #input
            -- If both columns are given then we can assume it's a partial block
            if scol and ecol then
                input[1] = input[1]:sub(1, scol - 1) .. input[1]:sub(scol - 1 + left_len, -1)
                if n == 1 then ecol = ecol - left_len end
                input[n] = input[n]:sub(1, ecol - right_len) .. input[n]:sub(ecol + 1, -1)
            else
                input[1] = input[1]:gsub('^(%s*)' .. lpattern, '%1')
                input[n] = input[n]:gsub(rpattern .. '$', '')
            end
            return input
        end
end

---Check if the given string is commented or not
---
---If given {string[]} to the closure, it will check the first and last line
---with LHS and RHS of commentstring respectively else it will check the given
---line with LHS and RHS (if given) of the commenstring
---@param left string Left side of the commentstring
---@param right string Right side of the commentstring
---@param padding boolean Is padding enabled?
---@param scol? integer Starting column
---@param ecol? integer Ending column
---@return fun(line: string|string[]):boolean
function U.is_commented(left, right, padding, scol, ecol)
    local padpattern = U.get_padpat(padding)
    local ll = U.is_empty(left) and left or '^%s*' .. vim.pesc(left) .. padpattern
    local rr = U.is_empty(right) and right or padpattern .. vim.pesc(right) .. '$'
    local pattern = ll .. '.-' .. rr

    return function(input)
        ------------------
        -- for linewise --
        ------------------
        if type(input) == 'string' then
            return input:find(pattern) ~= nil
        end

        -------------------
        -- for blockwise --
        -------------------
        if not scol then scol = 1 end
        if not ecol then ecol = #input[#input] end
        return (input[1]:sub(scol, -1):find(ll)
            and input[#input]:sub(1, ecol):find(rr)) ~= nil
    end
end

---@private
---Error handler
---@param ... unknown
function U.catch(fn, ...)
    xpcall(fn, function(err)
        vim.notify(string.format('[Comment.nvim] %s', err.msg), vim.log.levels.WARN)
    end, ...)
end

return U



