"Reloads vimrc after saving but keep cursor position
if !exists('*ReloadVimrc')
   fun! ReloadVimrc()
       let save_cursor = getcurpos()
       source $MYVIMRC
       call setpos(".", save_cursor)
   endfun
endif
autocmd! BufWritePost $MYVIMRC call ReloadVimrc()

" Enable true color
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

" linter invoking
nnoremap <Leader>e :call LintFileExternal()<CR>

function! LintFileExternal()
  if (&ft=='typescript' || &ft=='typescript.tsx' || &ft=='typescriptreact')
    let linterBuffType = 'eslint'

    " Recursively find local eslint, otherwise use global binary
    let localESLint = findfile('node_modules/.bin/eslint', '.;')

    if (!empty(localESLint))
      let linter = system(localESLint . " " . bufname("%") . " 2>&1")
    else
      let linter = system("eslint " . bufname("%") . " 2>&1")
    endif
  elseif (&ft=='elixir')
    let linterBuffType = 'elixir credo'

    let linter = system("mix credo")
  endif

  if (exists('linter'))
    split __Linter_Results__
    normal! ggdG
    setlocal buftype=nofile
    setlocal bufhidden=delete
    let &filetype = linterBuffType
    call append(0, split(linter, '\v\n'))
  else
    echo "File type not supported by available linters"
  endif
endfunction
