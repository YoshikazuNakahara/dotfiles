source ~/.zplug/init.zsh
# Pure theme
zplug mafredri/zsh-async, from:github
zplug sindresorhus/pure, use:pure.zsh, from:github, as:theme
PURE_POMNPT_SYMBOL='>'
PURE_GIT_DOWN_ARROW='+'
PURE_GIT_DOWN_ARROW='-'

# Plugins
## fish-like actions.
zplug 'zsh-users/zsh-autosuggestions'
zplug 'zsh-users/zsh-syntax-highlighting'
zplug "zsh-users/zsh-history-substring-search"
# enhanced completions
zplug 'zsh-users/zsh-completions'
# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# basic setting
## 矢印キーのインターフェイスを使って自動補完
zstyle ':completion:*' menu select
## sudo 補完
zstyle ':completion::complete:*' gain-privileges 1
## エイリアスでコマンドラインの自動補完
setopt completealiases
## 履歴に同じ行が重複するのを避ける
setopt HIST_IGNORE_DUPS
# cd 短縮
setopt auto_cd
## 現在の入力で始まる過去のコマンドだけが表示
[[ -n "${key[PageUp]}"   ]]  && bindkey  "${key[PageUp]}"    history-beginning-search-backward
[[ -n "${key[PageDown]}" ]]  && bindkey  "${key[PageDown]}"  history-beginning-search-forward
## ディレクトリスタック
DIRSTACKFILE="$HOME/.cache/zsh/dirs"
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
  dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
  [[ -d $dirstack[1] ]] && cd $dirstack[1]
fi
chpwd() {
  print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
}
DIRSTACKSIZE=20
setopt autopushd pushdsilent pushdtohome
### Remove duplicate entries
setopt pushdignoredups
### This reverts the +/- operators.
setopt pushdminus
# 拡張 glob を有効に
setopt extended_glob
WORDCHARS='*?_-.[]~&;!#$%^()<>'
# 履歴検索
# 入力したものが履歴に含まれてたら，古いものを削除
setopt hist_ignore_all_dups
## 空白で始まったら，履歴に追加しない
setopt hist_ignore_space
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

#Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi
# help
autoload -Uz run-help
unalias run-help
alias help=run-help
autoload -Uz run-help-git
autoload -Uz run-help-ip
autoload -Uz run-help-openssl
autoload -Uz run-help-p4
autoload -Uz run-help-sudo
autoload -Uz run-help-svk
autoload -Uz run-help-svn
# Then, source plugins and add commands to $PATH
zplug load --verbose
