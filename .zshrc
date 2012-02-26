## includeings
# parameter
source ~/.zsh.d/zshrc
# package.zsh
source ~/.zsh.d/package.zsh

export LISTMAX=10000

# completion
source ~/.zsh.d/completion.zsh

# candidate list compact
setopt listpacked

zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34' completer _oldlist _complete _match _ignored _approximate
zstyle ':zle:*' word-chars " -~()/:@+|"
zstyle ':zle:*' word-style unspecified
autoload -Uz select-word-style
select-word-style bash

# grep
## デフォルトオプションの設定
export GREP_OPTIONS
### バイナリファイルにはマッチさせない。
GREP_OPTIONS="--binary-files=without-match"
### grep対象としてディレクトリを指定したらディレクトリ内を再帰的にgrepする。
GREP_OPTIONS="--directories=recurse $GREP_OPTIONS"
### 拡張子が.tmpのファイルは無視する。
GREP_OPTIONS="--exclude=\*.tmp $GREP_OPTIONS"

## 管理用ディレクトリを無視する。
if grep --help | grep -q -- --exclude-dir; then
    GREP_OPTIONS="--exclude-dir=.svn $GREP_OPTIONS"
    GREP_OPTIONS="--exclude-dir=.git $GREP_OPTIONS"
    GREP_OPTIONS="--exclude-dir=.deps $GREP_OPTIONS"
    GREP_OPTIONS="--exclude-dir=.libs $GREP_OPTIONS"
fi
### 可能なら色を付ける。
if grep --help | grep -q -- --color; then
    GREP_OPTIONS="--color=auto $GREP_OPTIONS"
fi

# 単語単位での区切り
export WORDCHARS='*?_-[]~=&;!#$%^(){}<>'
bindkey ";5C" forward-word
bindkey "[1;5C" forward-word
bindkey "5C" forward-word
bindkey ";5D" backward-word
bindkey "[1;5D" backward-word
bindkey "5D" backward-word

bindkey ";3C" forward-word
bindkey "[1;3C" forward-word
bindkey "3C" forward-word
bindkey ";3D" backward-word
bindkey "[1;3D" backward-word
bindkey "3D" backward-word

# encolor make
e_normal=`echo -e "\033[0;30m"`
e_RED=`echo -e "\033[1;31m"`
e_BLUE=`echo -e "\033[1;36m"`

function make() {
    LANG=C command make "$@" 2>&1 | sed -e "s@[Ee]rror:.*@$e_RED&$e_normal@g" -e "s@cannot\sfind.*@$e_RED&$e_normal@g" -e "s@[Ww]arning:.*@$e_BLUE&$e_normal@g"
}
function cwaf() {
    LANG=C command ./waf "$@" 2>&1 | sed -e "s@[Ee]rror:.*@$e_RED&$e_normal@g" -e "s@cannot\sfind.*@$e_RED&$e_normal@g" -e "s@[Ww]arning:.*@$e_BLUE&$e_normal@g"
}

setopt prompt_subst

# environment variables
export LANG=ja_JP.UTF-8
eval `dircolors`
# JAVA
export JAVA_HOME='/usr/lib/jvm/java-1.6.0-openjdk'

# history
HISTFILE=~/.zsh_history
HISTSIZE=10000000
SAVEHIST=10000000
setopt extended_history
setopt hist_ignore_space
setopt hist_ignore_all_dups     # ignore duplication command history list
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history         # コマンド履歴ファイルを共有する
setopt no_flow_control
setopt append_history        # 履歴を追加 (毎回 .zsh_history を作るのではなく)
setopt inc_append_history    # 履歴をインクリメンタルに追加
setopt hist_no_store         # historyコマンドは履歴に登録しない
setopt hist_reduce_blanks    # 余分な空白は詰めて記録

# emacs like
bindkey -e

# history with C-p C-n
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# terminal title option
case "${TERM}" in
kterm*|xterm)
    precmd() {
        echo -ne "\033]0;${USER}@${PWD}\007"
    }
    ;;
esac
setopt long_list_jobs

# move with directory name
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt auto_menu
setopt magic_equal_subst
setopt print_eight_bit

# other...
setopt correct
setopt list_packed
setopt nolistbeep
autoload math functions
setopt complete_aliases


# other
umask 022

setopt nolistbeep
unsetopt promptcr

setopt no_hup
setopt notify

setopt rm_star_wait

alias notify="notify-send success! -i ~/Dropbox/ok_icon.png || notify-send Failed... -i ~/Dropbox/ng_icon.png"
alias fing="find / 2> /dev/null | grep "
alias apt-install="apt-get install"
alias apt-search="apt-cache search"

export IDEA_JDK='/usr/lib/jvm/jdk1.6.0_23'
export JDK_HOME='/usr/lib/jvm/jdk1.6.0_23'


xmodmap ~/.Xmodmap


# perf aliases
BRANCHE='branches,branch-misses,branch-loads,branch-load-misses'
BRANCHER='branches:r,branch-misses:r,branch-loads:r,branch-load-misses:r'
CACHE='cache-references,cache-misses'
L1='L1-dcache-loads,L1-dcache-load-misses,L1-dcache-stores,L1-dcache-store-misses,L1-dcache-prefetches,L1-dcache-prefetch-misses,L1-icache-loads,L1-icache-load-misses,L1-icache-prefetches,L1-icache-prefetch-misses'
TLB='dTLB-loads,dTLB-load-misses,dTLB-stores,dTLB-store-misses,dTLB-prefetches,dTLB-prefetch-misses'
SCHED='sched:sched_stat_wait:r,sched:sched_stat_sleep:r,sched:sched_stat_iowait:r'
ALL=$BRANCHE,$CACHE,$L1,$TLB,$SCHED

# gtest color
export GTEST_COLOR=yes
alias akiyama='parallel-ssh -h /home/kumazaki/clusters/akiyama-hosts'

# use editor
export EDITOR=emacsclient
setopt ignore_eof
export LESSOPEN='| /usr/share/source-highlight/src-hilite-lesspipe.sh %s'
export TEXINPUTS=./:~/Dropbox/texstyle//:/usr/share/texmf-texlifve/tex//

# prompt setting
alias vc_prompt='vcprompt -f "%b:%m%u"'
export PROMPT="%B%n%b> $ "
export SPROMPT="%{${fg[red]}%}correct: %R -> %r [nyae]? %{${reset_color}%}"
export PROMPT2="%{${fg[blue]}%}%_> %{${reset_color}%}"

function update_rprompt() {
    local name st color gitdir action
    if [[ "$PWD" =~ '/\.git(/.*)?$' ]]; then
        return
    fi
    name=`git branch 2> /dev/null | grep '^\*' | cut -b 3-`
    if [[ -z $name ]]; then
        return
    fi

    gitdir=`git rev-parse --git-dir 2> /dev/null`
    #action=`VCS_INFO_git_getaction "$gitdir"` && action="($action)"

    st=`git status 2> /dev/null`
    if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
        color=%F{green}
    elif [[ -n `echo "$st" | grep "^nothing added"` ]]; then
        color=%F{yellow}
    elif [[ -n `echo "$st" | grep "^# Untracked"` ]]; then
        color=%B%F{red}
    else
        color=%F{red}
    fi

    echo "$color$name$action%f%b "
}

RPROMPT='[%~:`update_rprompt`]'


#rprompt_prompt
PROMPT_COMMAND=update_rprompt

ppchpwd_functions=($chpwd_functions dirs)


fpath=(~/.pythonbrew/venvs $fpath)
autoload -U ~/.pythonbrew/venvs/*(:t)

zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''

export LESS='-R'
export LESSOPEN='| /usr/share/source-highlight/src-hilite-lesspipe.sh %s'
