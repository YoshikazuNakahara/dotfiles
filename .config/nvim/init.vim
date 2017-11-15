" An example for a vimrc file.
"
" To use it, copy it to
"     for Unix:     $HOME/.config/nvim/init.vim
"     for Windows:  %LOCALAPPDATA%\nvim\init.vim

"set backup             " keep a backup file (restore to previous version)
"set undofile           " keep an undo file (undo changes after closing)
" vim 養成
noremap <unique> <Up> :<C-U>qall!<CR>
noremap! <unique> <Up> <Esc>:qall!<CR>
noremap <unique> <Down> :<C-U>qall!<CR>
noremap! <unique> <Down> <Esc>:qall!<CR>
noremap <unique> <Left> :<C-U>qall!<CR>
noremap! <unique> <Left> <Esc>:qall!<CR>
noremap <unique> <Right> :<C-U>qall!<CR>
noremap! <unique> <Right> <Esc>:qall!<CR>
set ruler              " show the cursor position all the time
set showcmd            " display incomplete commands

"clip board
set clipboard+=unnamedplus

" Don't use Ex mode, use Q for formatting
noremap Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" I like highlighting strings inside C comments.
let c_comment_strings=1

" Put these in an autocmd group, so that we can delete them easily.
augroup vimrcEx
  autocmd!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  autocmd BufReadPost *
    \ if line("'\"") >= 1 && line("'\"") <= line("$") |
    \   execute "normal! g`\"" |
    \ endif

augroup END

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set buftype=nofile | read ++edit # | 0d_ | diffthis
                 \ | wincmd p | diffthis
endif
set wildmode=longest:full,full
cnoremap <C-a> <Home>
" 一文字戻る
cnoremap <C-b> <Left>
" カーソルの下の文字を削除
cnoremap <C-d> <Del>
" 行末へ移動
cnoremap <C-e> <End>
" 前の単語へ移動
cnoremap <M-b> <S-Left>
" 次の単語へ移動
cnoremap <M-f> <S-Right>
" ファイル名補完を有効にする
set wildmenu
" コマンドラインの履歴を1000件保存する
set history=1000
" コマンド履歴を表示する command -> q:
" 検索履歴を表示する command -> 前方検索 q/ 後方検索 q?
" コマンドラインに履歴を送る -> C-c
" GUI font config
" show font name -> :set guifont
" gui font setting -> :set guifont=<fontname>:h<size>
" 括弧の対応
set matchpairs+=<:>
" 検索がファイル末尾まで進んだら、ファイル先頭から再び検索する。（有効:wrapscan/無効:nowrapscan）
set wrapscan
" help language are jp, en.
set helplang=ja,en
" 左右のカーソル移動で行を跨いで移動
set whichwrap=b,s,h,l,<,>,[,],~
" バックスペースでの削除をいつでも有効にする
set backspace=indent,eol,start
" 大文字小文字を無視
set ignorecase
" ignorecaseと合わせることで，小文字入力の時のみ大文字小文字を無視
set smartcase
" インクリメンタルサーチ
set incsearch
" 検索結果をハイライト
set hlsearch
" ESCキー連打でハイライトを消す
nnoremap <silent> <ESC><ESC> :nohlsearch<CR>
" インサートモードに入る時に自動でコメントアウトされないようにする
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" 変換候補で一度に表示される数を設定する
set pumheight=5
" タブの代わりに空白を使う
set expandtab
" タブの幅
set tabstop=4
" 自動インデントでずれる幅
set shiftwidth=4
" 連続した空白に対してタブキーやバックスペースキーでカーソルが動く幅（デフォルトでは無効: 0）
set softtabstop=2
" 改行時に前の行のインデントを継続しない
set noautoindent
" 改行時に入力された行の末尾に合わせて次の行のインデントを増減する
set smartindent
"左下のインサートモードなどの文字を非表示にする
set noshowmode
"スクロールバーを消す
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L
set guioptions-=b
"下部のステータスラインを常に表示
set laststatus=2
" line number
set number
" ソースコードを折畳み表示する
set foldmethod=marker
" manual: 手動で折畳を定義する
" indent: インデントの数を折畳のレベル(深さ)とする
" expr:   折畳を定義する式を指定する
" syntax: 構文強調により折畳を定義する
" diff:   変更されていないテキストを折畳対象とする
" marker: テキスト中の印で折畳を定義する
" コメントの形式をスラッシュ2つの形式に変更
set commentstring=//%s
" カンマで区切って、マーカー文字列の対を指定する
set foldmarker=[[[,]]]
" 編集行の行番号を常にハイライトする
set cursorline " 現在の行をハイライト
hi clear CursorLine " 上と合わせることで行番号のみハイライト
" ビープ音を消す
set vb t_vb=
" CLIでVimを使用した際に生じるEscキーのディレイを解消
if !has('gui_running')
    set timeout timeoutlen=1000 ttimeoutlen=50
endif

" color scheme
filetype on
" jjでエスケープ
inoremap <silent> jj <ESC>
" uim
" 「日本語入力固定モード」切替キー
inoremap <silent> <C-l> <C-r>=IMState('FixMode')<CR>
" GVimの時だけ「日本語入力固定モード」のvi協調モードを無効化
let IM_vi_CooperativeMode = has('gui_running') ? 0 : 1

" <ESC>押下後のIM切替開始までの反応が遅い場合はttimeoutlenを短く設定してみてください(ミリ秒)
set timeout timeoutlen=3000 ttimeoutlen=100

" 「日本語入力固定モード」がオンの場合、ステータス行にメッセージ表示
set statusline+=%{IMStatus('[日本語固定]')}

" comment
highlight Comment cterm=italic ctermfg=6
" color
highlight Search ctermfg=black ctermbg=white
" color
highlight Folded ctermfg=black ctermbg=green
highlight FoldColumn ctermfg=black ctermbg=green
highlight DiffAdd ctermfg=black ctermbg=81
highlight DiffChange ctermfg=black ctermbg=225
highlight DiffDelete ctermfg=black ctermbg=3
highlight SignColumn ctermfg=black ctermbg=5
highlight ShellBad ctermfg=black ctermbg=112
highlight ShellRare ctermfg=black ctermbg=225

" plugin manager setting
" プラグインが実際にインストールされるディレクトリ
let s:dein_dir = expand('~/.config/nvim')
" dein.vim 本体
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'


" dein.vim がなければ github から落としてくる
if &runtimepath !~# '/dein.vim'
  if !isdirectory(s:dein_repo_dir)
    execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo_dir
  endif
  execute 'set runtimepath^=' . fnamemodify(s:dein_repo_dir, ':p')
endif

" 設定開始
if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

    let s:toml_dir = s:dein_dir . '/dein'

    call dein#load_toml(s:toml_dir . '/plugins.toml', {'lazy': 0})
  "  call dein#load_toml(s:toml_dir . '/lazy.toml', {'lazy': 1})
  "  if has('nvim')
  "      call dein#load_toml(s:toml_dir . '/neovim.toml', {'lazy': 1})
  "  endif
  " プラグインリストを収めた TOML ファイル
  " 予め TOML ファイル（後述）を用意しておく
  " TOML を読み込み、キャッシュしておく

  " 設定終了
  call dein#end()
  call dein#save_state()
endif

" もし、未インストールものものがあったらインストール
if dein#check_install()
  call dein#install()
endif
set helplang=ja,en
let g:tex_conceal=''
