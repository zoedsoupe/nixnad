"MAPPINGS

inoremap <C-a> <Home>
inoremap <C-e> <End>
nnoremap <C-a> <Home>
nnoremap <C-e> <End>

" delete text
map <BS> c

"save files
map <C-s> :w<CR>

"navigate buffers
nnoremap <C-g> :buffers<CR>:buffer<Space> 

"move among buffers with CTRL
map <C-L> :bnext<CR>
map <C-K> :bprev<CR>

"exit insert mode
inoremap <C-x> <Esc>

"set highlighting off after regex 
nnoremap <CR> :noh<CR><CR>

