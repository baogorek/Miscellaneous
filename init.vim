call plug#begin('C:\Users\baogo\AppData\Local\nvim\plugged')

Plug 'Vigemus/iron.nvim'

call plug#end()
luafile C:\Users\baogo\AppData\Local\nvim\plugins.lua

function! Show_position()
    let new_list = [getpos("'<")[1], getpos("'>")[1]]
    return new_list
endfunction

map <localleader>t <Cmd>lua my_table = vim.api.nvim_buf_get_lines(0, 0, -1, false); for i = 1, table.getn(my_table) do require("iron").core.send(vim.api.nvim_buf_get_option(0, "ft"), my_table[i]) end; require("iron").core.send(vim.api.nvim_buf_get_option(0, "ft"), "\n\r")<Cr>

nmap <localleader>s <Plug>(iron-send-line)

vmap <localleader>s :lua lower, upper = unpack(vim.api.nvim_call_function("Show_position", {})); my_table = vim.api.nvim_buf_get_lines(0, lower - 1, upper, false); for i = 1, table.getn(my_table) do require("iron").core.send(vim.api.nvim_buf_get_option(0, "ft"), my_table[i]) end; require("iron").core.send(vim.api.nvim_buf_get_option(0, "ft"), "\n\r");<Cr><Cr>
