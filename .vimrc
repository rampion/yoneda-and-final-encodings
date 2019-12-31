set makeprg=cabal
nnoremap <Leader>m :make build<CR>
nnoremap <Leader>t :make test<CR>
nnoremap <Leader>p :!pandoc --from gfm --standalone > README.html README.md --metadata title="Yoneda and Final Encodings"<CR>
