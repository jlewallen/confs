" ----------------------------------------------------------------------------
" $Id: $
"
" --------------------------------------------------------------------------*/

" Some handy things to keep in mind while editing files:
" set ff=unix
" set ff=dos
" http://larc.ee.nthu.edu.tw/~cthuang/vim/
" http://larc.ee.nthu.edu.tw/~cthuang/vim/vim-living.html

" autocomplete from file
" ctrl-n
" ctrl-p

" Turn on syntax hilighting, it's a joy to have enabled when editing source
" code and all that other good stuff.
set background=light
set nocompatible
set undolevels=100
set autoindent
set smarttab
set expandtab
set backspace=2
set shiftwidth=4
set tabstop=4
set showcmd
set novisualbell
set cmdheight=2
set ruler
set history=100
set wildmenu
set shortmess=a
set nohlsearch
set joinspaces
set wildmenu
set showfulltag
set display+=lastline
set sidescroll=1
set sidescrolloff=1
set hidden
set nobackup
set writebackup
set backupext=.bak
set viminfo+=%
set cindent
set cino=:0,g0,t0,+1,(0
set foldlevel=999
set nowrap
set textwidth=0
 " enable per-directory .vimrc files
set exrc
" disable unsafe commands in local .vimrc files
set secure

" Exclusive selection, this is better.
if exists('&selection')
  set selection=exclusive
endif

" Setup Windows specific directories and such.
if has("win32")
  set backupdir=$TEMP
  set directory=$TEMP
else
  set backupdir=~/tmp
  set directory=~/tmp
endif

syntax on
" ----------------------------------------------------------------------------
" Explorer settings
let g:explDetailedList = 1

" ----------------------------------------------------------------------------
" SelectBuf settings
let g:selBufAlwaysShowDetails = 1

" ----------------------------------------------------------------------------
" Manual Folding (use zf%)
" set foldmethod=manual
"
" Marker Folding
" set foldmethod=marker
" set foldmarker={,}
"
" Indention Folding (Python)
" set foldmethod=indent

au BufReadPost *.cs   syn region myFold start="{" end="}" transparent fold
au BufReadPost *.cs   syn sync fromstart
au BufReadPost *.cs   set foldmethod=syntax

au BufReadPost *.c    syn region myFold start="{" end="}" transparent fold
au BufReadPost *.c    syn sync fromstart
au BufReadPost *.c    set foldmethod=syntax

" Toggle fold state between closed and opened.
"
" If there is no fold at current line, just moves forward.
" If it is present, reverse it's state.
function! ToggleFold()
  if foldlevel('.') == 0
    normal! l
  else
    if foldclosed('.') < 0
      . foldclose
    else
      . foldopen
    endif
  endif
  " Clear status line
  echo
endfunction

" ----------------------------------------------------------------------------
" Mappings
nmap <tab> :if &modifiable && !&readonly && &modified <cr> :w<cr> :endif<cr> :bn<cr>
nmap <s-tab> :if &modifiable && !&readonly && &modified <cr> :w<cr> :endif<cr> :bp<cr>

nmap <Tab> <Plug>SelectBuf
nmap <C-B> <Plug>SelectBuf

" Make Y the analog of D
map Y y$
 
" Eh?
noremap <C-g>  i<Space><Esc>r

" ----------------------------------------------------------------------------
" Options specific to when the GUI is running.
if has("gui_running")
  " Use a good font under win32, the others are pretty ugly.
  if has("win32")
    set guifont=Lucida_Console:h9:cANSI
  else
    set guifont=fixed
  endif

  set mousehide
  set noguipty
  set guioptions-=T
  au GUIEnter * win 140 64

  " Only works in GUI mode, ESC is used for other things while in console
  " (arrows stop working, things just break)
  " nmap <esc> :
endif

" ----------------------------------------------------------------------------
" Common settings for any source code editing.

autocmd FileType c         call FT_CodeAll()
autocmd FileType cpp       call FT_CodeAll()
autocmd FileType java      call FT_CodeAll()
autocmd FileType make      call FT_CodeAll()
autocmd FileType perl      call FT_CodeAll()
autocmd FileType lisp      call FT_CodeAll()
autocmd FileType html,sgml call FT_CodeAll()

function! FT_CodeAll()
endfunction

autocmd BufRead *.txt     set textwidth=78

" ----------------------------------------------------------------------------
"  Automatically set some cool file types, we like these.
augroup filetype
  au!
  au! BufRead,BufNewFile *.csproj  set filetype=xml
  au! BufRead,BufNewFile *.msbuild set filetype=xml
  au! BufRead,BufNewFile *.proj    set filetype=xml
  au! BufRead,BufNewFile *.sql     set filetype=sql
  au! BufRead,BufNewFile *.ps1     set filetype=Msh
  au! BufRead,BufNewFile *.psql    set filetype=sql
  au! BufRead,BufNewFile *.mk      set filetype=makefile
  au! BufRead,BufNewFile *.build   set filetype=sgml
  au! BufRead,BufNewFile *.lzx     set filetype=lzx
  au! BufNewFile,BufRead *.gradle  set filetype=groovy
augroup END

" ----------------------------------------------------------------------------
colorscheme wombat
colorscheme darkocean

