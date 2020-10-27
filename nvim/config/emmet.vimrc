"emmet trigger key
let g:user_emmet_leader_key=','
let g:user_emmet_install_global = 0
let g:user_emmet_mode='a'
autocmd FileType html,typescriptreact EmmetInstall

let g:user_emmet_settings = {
  \   'typescript': {
  \     'extends': 'tsx',
  \   },
  \ }

