---@mod comment.opfunc Operator-mode API
---@brief [[
---Underlying functions that powers the |comment.api.toggle|, |comment.api.comment|,
---and |comment.api.uncomment| lua API.
---@brief ]]

local Utils = require('Comment.utils')
local Config = require('Comment.config')

local Op = {}

---Vim operator-mode motion enum. Read |:map-operator|
---@alias OpMotion
---| '"line"' # Vertical motion
---| '"char"' # Horizontal motion
---| '"v"' # Visual Block motion
---| '"V"' # Visual Line motion

---Common operatorfunc callback
---This function contains the core logic for comment/uncomment
---@param motion? OpMotion
---If given 'nil', it'll only (un)comment
---the current line
---@param cfg CommentConfig
---@param cmode integer See |comment.utils.cmode|
---@param ctype integer See |comment.utils.ctype|
function Op.opfunc(motion, cfg, cmode, ctype)
    local range = Utils.get_region(motion)
    local cmotion = motion == nil and Utils.cmotion.line or Utils.cmotion[motion]

    -- If we are doing char or visual motion on the same line
    -- then we would probably want block comment instead of line comment
    local is_partial = cmotion == Utils.cmotion.char or cmotion == Utils.cmotion.v
    -- local is_blockx = is_partial and range.srow == range.erow

    local lines = Utils.get_lines(range)

    -- sometimes there might be a case when there are no lines
    -- like, executing a text object returns nothing
    if Utils.is_empty(lines) then
        return
    end

    ---@type CommentCtx
    local ctx = {
        cmode = cmode,
        cmotion = cmotion,
        ctype = ctype,
        range = range,
    }

    local lcs, rcs = Utils.parse_cstr(cfg, ctx)

    ---@type OpFnParams
    local params = {
        cfg = cfg,
        lines = lines,
        lcs = lcs,
        rcs = rcs,
        cmode = cmode,
        range = range,
    }

    -- if motion ~= nil and (is_blockx or ctype == Utils.ctype.blockwise) then
    if ctype == Utils.ctype.blockwise then
        ctx.cmode = Op.blockwise(params, is_partial)
    else
        ctx.cmode = Op.linewise(params)
    end

    -- We only need to restore cursor if both sticky and position are available
    -- As this function is also called for visual mapping where we are not storing the position
    --
    -- And I found out that if someone presses `gc` but doesn't provide operators and
    -- does visual comments then cursor jumps to previous stored position. Thus the check for visual modes
    if cfg.sticky and Config.position and cmotion ~= Utils.cmotion.v and cmotion ~= Utils.cmotion.V then
        vim.api.nvim_win_set_cursor(0, Config.position)
        Config.position = nil
    end

    Utils.is_fn(cfg.post_hook, ctx)
end

---Line commenting with count
---@param count integer Value of |v:count|
---@param cfg CommentConfig
---@param cmode integer See |comment.utils.cmode|
---@param ctype integer See |comment.utils.ctype|
function Op.count(count, cfg, cmode, ctype)
    local lines, range = Utils.get_count_lines(count)

    ---@type CommentCtx
    local ctx = {
        cmode = cmode,
        cmotion = Utils.cmotion.line,
        ctype = ctype,
        range = range,
    }
    local lcs, rcs = Utils.parse_cstr(cfg, ctx)

    ---@type OpFnParams
    local params = {
        cfg = cfg,
        cmode = ctx.cmode,
        lines = lines,
        lcs = lcs,
        rcs = rcs,
        range = range,
    }

    if ctype == Utils.ctype.blockwise then
        ctx.cmode = Op.blockwise(params)
    else
        ctx.cmode = Op.linewise(params)
    end

    Utils.is_fn(cfg.post_hook, ctx)
end

---Operator-mode function parameters
---@class OpFnParams
---@field cfg CommentConfig
---@field cmode integer See |comment.utils.cmode|
---@field lines string[] List of lines
---@field rcs string RHS of commentstring
---@field lcs string LHS of commentstring
---@field range CommentRange

---Line commenting
---@param param OpFnParams
---@return integer _ Returns a calculated comment mode
function Op.linewise(param)
    local pattern = Utils.is_fn(param.cfg.ignore)
    local padding = Utils.is_fn(param.cfg.padding)
    local check_comment = Utils.is_commented(param.lcs, param.rcs, padding)

    -- While commenting a region, there could be lines being both commented and non-commented
    -- So, if any line is uncommented then we should comment the whole block or vise-versa
    local cmode = Utils.cmode.uncomment

    ---When commenting multiple line, it is to be expected that indentation should be preserved
    ---So, When looping over multiple lines we need to store the indentation of the minimum length (except empty line)
    ---Which will be used to semantically comment rest of the lines
    local min_indent, tabbed = -1, false

    -- If the given cmode is uncomment then we actually don't want to compute the cmode or min_indent
    if param.cmode ~= Utils.cmode.uncomment then
        for _, line in ipairs(param.lines) do
            if Utils.ignore(line, pattern) then
                goto continue
            end
            if cmode == Utils.cmode.uncomment and param.cmode == Utils.cmode.toggle and (not check_comment(line)) then
                cmode = Utils.cmode.comment
            end

            if not Utils.is_empty(line) and param.cmode ~= Utils.cmode.uncomment then
                local _, len = string.find(line, '^%s*')
                if len and (min_indent == -1 or min_indent > len) then
                    min_indent, tabbed = len, (string.find(line, '^\t') ~= nil)
                end
            end
            ::continue::
        end
    end

    -- If the comment mode given is not toggle than force that mode
    if param.cmode ~= Utils.cmode.toggle then
        cmode = param.cmode
    end

    if cmode == Utils.cmode.uncomment then
        local uncomment = Utils.uncommenter(param.lcs, param.rcs, padding)
        for i, line in ipairs(param.lines) do
            if not Utils.ignore(line, pattern) then
                param.lines[i] = uncomment(line) --[[@as string]]
            end
        end
    else
        local comment = Utils.commenter(param.lcs, param.rcs, padding, min_indent+1, nil, tabbed)
        for i, line in ipairs(param.lines) do
            if not Utils.ignore(line, pattern) then
                param.lines[i] = comment(line) --[[@as string]]
            end
        end
    end

    vim.api.nvim_buf_set_lines(0, param.range.srow - 1, param.range.erow, false, param.lines)

    return cmode
end

---Full/Partial/Current-Line Block commenting
---@param param OpFnParams
---@param partial? boolean Comment the partial region (visual mode)
---@return integer _ Returns a calculated comment mode
function Op.blockwise(param, partial)
    local is_x = #param.lines == 1 -- current-line blockwise
    -- local input = is_x and param.lines[1] or param.lines
    local input = param.lines

    local padding = Utils.is_fn(param.cfg.padding)

    local scol, ecol = nil, nil
    -- if is_x or partial then
    if partial then
        scol, ecol = param.range.scol, param.range.ecol
    end
    -- if V-mode then scol and ecol are empty
    -- if v-mode or char-mode then scol and ecol correspond to the selection

    -- If given mode is toggle then determine whether to comment or not
    local cmode = param.cmode
    if cmode == Utils.cmode.toggle then
        local is_cmt = Utils.is_commented(param.lcs, param.rcs, padding, scol, ecol)(input)
        cmode = is_cmt and Utils.cmode.uncomment or Utils.cmode.comment
    end

    if cmode == Utils.cmode.uncomment then
        input = Utils.uncommenter(param.lcs, param.rcs, padding, scol, ecol)(input)
    else
        input = Utils.commenter(param.lcs, param.rcs, padding, scol, ecol)(input)
    end

    vim.api.nvim_buf_set_lines(0, param.range.srow - 1, param.range.erow, false, input)

    return cmode
end

return Op

