let g:deoplete#enable_at_startup = 1

call deoplete#custom#option({
    \ 'auto_complete_delay': 1,
    \ 'smart_case': v:true,
    \ })

call deoplete#custom#option('ignore_sources', {
      \ '_': ['buffer', 'around', 'member'],
      \ })

call deoplete#custom#source('_', 'dup', v:false)

call deoplete#custom#source('_', 'converters', [
    \ 'converter_remove_paren',
    \ 'converter_remove_overlap',
    \ 'converter_truncate_abbr',
    \ 'converter_truncate_info',
    \ 'converter_truncate_menu',
    \ 'converter_auto_delimiter'
    \])

inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <silent><expr> <C-Space> deoplete#manual_complete()
