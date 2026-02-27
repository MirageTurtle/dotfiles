#!/usr/bin/env bash
# remove fancy prompt when the $TERM is "dumb"
# this command should place in the first line of ~/.zshrc directly
# [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return 1

# alias for common commands
alias mv='mv -i'
default_proxy_host="127.0.0.1"
default_proxy_port="2333"
# alias proxy="export https_proxy=http://$proxy_host:$proxy_port http_proxy=http://$proxy_host:$proxy_port all_proxy=socks5://$proxy_host:$proxy_port"
function proxy() {
    local proxy_host="$default_proxy_host"
    local proxy_port="$default_proxy_port"
    local socks5_port="$default_proxy_port"
    # -p is for port, -h is for host
    # -s is for socks5 port
    while getopts "p:h:s:" opt; do
        case $opt in
            p) proxy_port="$OPTARG" ;;
            h) proxy_host="$OPTARG" ;;
            s) socks5_port="$OPTARG" ;;
            *) echo "Usage: proxy [-p port] [-h host]" >&2; return 1 ;;
        esac
    done
    export http_proxy="http://$proxy_host:$proxy_port"
    export https_proxy="http://$proxy_host:$proxy_port"
    export all_proxy="socks5://$proxy_host:$socks5_port"
    export HTTP_PROXY="http://$proxy_host:$proxy_port"
    export HTTPS_PROXY="http://$proxy_host:$proxy_port"
    export ALL_PROXY="socks5://$proxy_host:$socks5_port"
}
alias unproxy="unset https_proxy http_proxy all_proxy HTTP_PROXY HTTPS_PROXY ALL_PROXY"
alias cp='cp -i'

# alias for modern unix commands
## eza
if command -v eza &> /dev/null; then
    alias ls-ls='/bin/ls'
    alias ls='eza --all --long --group --group-directories-first --icons --header --time-style long-iso'
fi
## bat
BAT_BIN=""
if command -v bat &> /dev/null; then
    BAT_BIN="bat"
elif command -v batcat &> /dev/null; then
    BAT_BIN="batcat"
fi
if [[ -n "$BAT_BIN" ]]; then
    alias cat-cat='/bin/cat'
    alias cat='$BAT_BIN'
fi
## fd (for ubuntu package binary)
if ! command -v fd &> /dev/null && command -v fdfind &> /dev/null; then
    alias fd="fdfind"
fi

# alias for copilot
if command -v gh &> /dev/null; then
    alias copilot='gh copilot suggest -t shell -- '
fi
QUICK_CHAT_SCHEME_HOST_PORT="http://100.64.0.1:13000"
QUICK_CHAT_ENDPOINT="${QUICK_CHAT_SCHEME_HOST_PORT}/api/v1/chat/completions"
QUICK_CHAT_MODEL="gpt-4o-mini"
function quickchat() {
    local prompt="$1"
    local payload
    shift
    payload=$(jq -n \
        --arg model "$QUICK_CHAT_MODEL" \
        --arg content "$prompt" \
        '{
            "model": $model,
            "messages": [{"role": "user", "content": $content}],
            "temperature": 0.7,
            "max_tokens": 800,
            "top_p": 0.9,
            "frequency_penalty": 0,
            "presence_penalty": 0
        }')
    curl -sS -X POST "$QUICK_CHAT_ENDPOINT" \
        -H "Content-Type: application/json" \
        -d "$payload" | jq -r '.choices[0].message.content'
}
alias qc="quickchat"

# env
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export TERM=xterm-256color
# editor
export EDITOR="emacs -nw"

# custom tmux command
function mtmux() {
    # If no parameter is passed, show interactive session selector
    # Otherwise, attach to or create a session with the name passed as the first parameter
    case "$1" in
        list)
            tmux list-sessions
            return 0
            ;;
        update-environment|update-env|env-update)
            mtmux_update_environment
            return 0
            ;;
        help)
            echo "Usage: mtmux [session_name]"
            echo "       mtmux list"
            echo "       mtmux update-environment|update-env|env-update"
            echo "       mtmux help"
            return 0
            ;;
        "")
            # Show interactive session selector using fzf
            selected_session=$(tmux list-sessions -F "#{session_name}" 2>/dev/null | fzf --height 40% --reverse --prompt="Select tmux session: ")
            if [ -z "$selected_session" ]; then
                # No session selected or fzf not available
                echo "No session selected. Exiting."
                return 0
            else
                target_session="$selected_session"
            fi
            ;;
        *)
            target_session="$1"
            ;;
    esac
    # now, the target_session variable is set
    # check if in a tmux session
    if [[ -n "$TMUX" ]]; then
	# If already in a tmux session, switch to the target session
	mtmux_in_session "$target_session"
    else
	# If not in a tmux session, attach to the target session or create it if it doesn't exist
	mtmux_out_session "$target_session"
    fi
}
function mtmux_in_session() {
    # switch to the target session if it exists, otherwise exit
    tmux switch-client -t "$1" 2>/dev/null || echo "Session '$1' does not exist."
}
function mtmux_out_session() {
    # attach to the target session if it exists, otherwise create a new session
    tmux attach-session -t "$1" 2>/dev/null || tmux new-session -s "$1"
}
function mtmux_update_environment() {
    # Update environment variables from tmux's global environment
    # This is useful when SSH_AUTH_SOCK or other env vars change
    local v var_name var_value
    while IFS= read -r v; do
        if [[ $v == -* ]]; then
            # Variable marked for removal (prefixed with -)
            unset "${v/#-/}"
        else
            # Split on first = to get variable name and value
            var_name="${v%%=*}"
            var_value="${v#*=}"
            # Export the variable
            export "$var_name=$var_value"
        fi
    done < <(tmux show-environment)
}


# alias for container
if command -v podman &> /dev/null; then
    alias tmpctr="podman run -it --rm --log-driver none" # thx to taoky
fi

# ripgrep
if command -v rg &> /dev/null; then
    # https://github.com/BurntSushi/ripgrep/issues/1352#issuecomment-1959071755
    rgnc(){ Q="$1"; shift; rg --pretty --colors match:none -o ".{0,50}$Q.{0,50}" "$@" | rg --passthru "$Q" ;}

    # Keep header line when filtering with rg
    # Usage: ps aux | rgh something
    rgh() {
        if [[ -z "$1" ]]; then
            echo "Usage: rgh <pattern>" >&2
            return 1
        fi
        { IFS= read -r header; printf '%s\n' "$header"; rg "$@"; }
    }
fi

# paste bin
## paste.rs
function pasters() {
    local file=${1:-/dev/stdin}
    # curl --data-binary @${file} https://paste.rs
    curl --data-binary @"${file}" https://paste.remnant.gay
    # if the file ext is .cast, add .cast to the end of the url
    [[ "$file" == *.cast ]] && echo -n ".cast"
    echo "" # add a newline for better readability
}

function pasters-delete() {
    local id=${1:-/dev/stdin}
    if [[ -z "$id" ]]; then
	echo "Usage: pasters-delete <paste_id>"
	return 1
    fi
    # curl -X DELETE https://paste.rs/${id}
    curl -X DELETE https://paste.remnant.gay/"${id}"
}
## 0x0.st
0x0() {
    local file=${1:-/dev/stdin}
    curl -F"file=@${file}" https://0x0.st
    echo "" # add a newline for better readability
}
