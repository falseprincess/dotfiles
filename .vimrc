"  __     __  ______  __       __  _______    ______  
" /  |   /  |/      |/  \     /  |/       \  /      \ 
" $$ |   $$ |$$$$$$/ $$  \   /$$ |$$$$$$$  |/$$$$$$  |
" $$ |   $$ |  $$ |  $$$  \ /$$$ |$$ |__$$ |$$ |  $$/ 
" $$  \ /$$/   $$ |  $$$$  /$$$$ |$$    $$< $$ |      
"  $$  /$$/    $$ |  $$ $$ $$/$$ |$$$$$$$  |$$ |   __ 
"   $$ $$/    _$$ |_ $$ |$$$/ $$ |$$ |  $$ |$$ \__/  |
"    $$$/    / $$   |$$ | $/  $$ |$$ |  $$ |$$    $$/ 
"     $/     $$$$$$/ $$/      $$/ $$/   $$/  $$$$$$/  
"
" FALSEPRINCESS' MAIN VIM CUSTOMIZATION FILE
" THIS CONFIG CAN BE FOUND AT: https://github.com/falseprincess/dotfiles
"
" Enable/Disable syntax.
syntax on

" Install vim-plug if not found
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
   \| PlugInstall --sync | source $MYVIMRC
    \| endif

" Calling some plugins
call plug#begin(expand('~/.vim/plugged'))

Plug 'itchyny/lightline.vim'
Plug 'arcticicestudio/nord-vim'
Plug 'scrooloose/nerdtree'
Plug 'ryanoasis/vim-devicons'
Plug 'sheerun/vim-polyglot'

call plug#end()

" Define Color scheme
colorscheme nord

" Starting Lightline / theming options.
set laststatus=2

if !has('gui_running')
	  set t_Co=256
  endif

let g:lightline = {
	      \ 'colorscheme': 'nord',
      \ }

" Scroll when cursor gets within 3 characters of top/bottom edge
set scrolloff=3

" Line numbers configuration.
set number

"Default encoding.
set encoding=UTF-8
