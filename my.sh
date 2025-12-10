#!/usr/bin/env zsh
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
    alias cat="$BAT_BIN"
fi
## fd (for ubuntu package binary)
if ! command -v fd &> /dev/null && command -v fdfind &> /dev/null; then
    alias fd="fdfind"
fi

# alias for copilot
if command -v gh &> /dev/null; then
    alias copilot='gh copilot suggest -t shell -- '
fi

# env
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export TERM=xterm-256color
# editor
export EDITOR="emacs -nw"

# sshfs
# Detect the operating system
function detect_os() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        echo "macOS"
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        echo "Linux"
    else
        echo "Unknown OS"
    fi
}

# Function to check if 'user_allow_other' is enabled in /etc/fuse.conf
function check_allow_other_enabled() {
    if grep -q "^user_allow_other" /etc/fuse.conf; then
        echo "user_allow_other is already enabled."
        return 0
    else
        echo "user_allow_other is NOT enabled."
        return 1
    fi
}

# Function to show the dry-run command and request sudo permission to fix it
function enable_allow_other() {
    echo "Dry-run: The following command will be run to enable 'user_allow_other' in /etc/fuse.conf:"
    echo "    sudo bash -c \"echo 'user_allow_other' >> /etc/fuse.conf\""

    echo -n "Do you want to proceed and enable 'user_allow_other'? (y/n): "
    read answer
    if [[ "$answer" == "y" || "$answer" == "Y" ]]; then
        # Request sudo permission and append 'user_allow_other' to /etc/fuse.conf
        sudo bash -c "echo 'user_allow_other' >> /etc/fuse.conf"

        if [[ $? -eq 0 ]]; then
            echo "'user_allow_other' has been successfully enabled in /etc/fuse.conf."
            return 0
        else
            echo "Failed to enable 'user_allow_other'. Please check your permissions or manually edit /etc/fuse.conf."
        fi
    else
        echo "Operation cancelled. 'user_allow_other' remains disabled."
    fi
    return 1
}

# Define OS-specific sshfs mount commands
function mysshfs() {
    local os=$(detect_os)
    if [[ "$os" == "macOS" ]]; then
        sshfs -C -o kill_on_unmount,reconnect,allow_other,defer_permissions,auto_cache,nolocalcaches,no_readahead "$1" "$2"
    elif [[ "$os" == "Linux" ]]; then
        mount_command="sshfs -C -o dir_cache=no,transform_symlinks,idmap=user,reconnect,allow_other \"$1\" \"$2\""
        eval $mount_command 2> /dev/null
        if [[ $? -ne 0 ]]; then
            local error_message=$(eval $mount_command 2>&1)
            echo "$error_message" > /tmp/test.txt
            if echo "$error_message" | grep "user_allow_other" | grep -q "/etc/fuse.conf"; then
                if ! check_allow_other_enabled; then
                    enable_allow_other
                    if [[ $? -eq 0 ]]; then
                        # retry the mount command
                        echo "Retrying the mount command..."
                        eval $mount_command
                    fi
                else
                    echo "$error_message"
                    return 2
                fi
            fi
        fi
    else
        echo "Unsupported OS"
        return 1
    fi
}

# Define OS-specific unmount commands
function mysshfs-umount() {
    local os=$(detect_os)
    if [[ "$os" == "macOS" ]]; then
        diskutil unmount force "$1"
    elif [[ "$os" == "Linux" ]]; then
        umount "$1"
    else
        echo "Unsupported OS"
        return 1
    fi
}

# Unmount all sshfs mounts
function mysshfs-umount-all() {
    local os=$(detect_os)
    if [[ "$os" == "macOS" ]]; then
        ps ux | grep sshfs | grep -v grep | awk '{print $NF}' | xargs -n1 diskutil unmount force
    elif [[ "$os" == "Linux" ]]; then
        ps ux | grep sshfs | grep -v grep | awk '{print $NF}' | xargs -n1 umount
    else
        echo "Unsupported OS"
        return 1
    fi
}

# Reconnect function
function mysshfs-reconnect() {
    mysshfs-umount "$2"
    mysshfs "$1" "$2"
}

# all-in-one function
function mtsshfs() {
    # $1: remote directory
    # $2: local directory
    # on macOS:
    # try to mount the remote directory to the local directory
    # if failed, try to unmount the local directory and mount again
    # mysshfs "$1" "$2" || mysshfs-reconnect "$1" "$2"
    # BUT on Linux:
    # it is likely that the twice mount will succeed
    # Judge the mount status now via `ps ux | grep sshfs | grep -q "$2"`
    # if not mounted, mount it
    # if mounted, remount it
    ps ux | grep sshfs | grep -q "$2" && mysshfs-reconnect "$1" "$2" || mysshfs "$1" "$2"
}

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
            unset ${v/#-/}
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
    curl --data-binary @${file} https://paste.rs
    echo "" # add a newline for better readability
}

function pasters-delete() {
    local id=${1:-/dev/stdin}
    if [[ -z "$id" ]]; then
	echo "Usage: paste-delete <paste_id>"
	return 1
    fi
    curl -X DELETE https://paste.rs/${id}
}
## 0x0.st
0x0() {
    local file=${1:-/dev/stdin}
    curl -F"file=@${file}" https://0x0.st
    echo "" # add a newline for better readability
}


