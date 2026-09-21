#!/usr/bin/env bash
# Source this file to define proxy-system, unproxy-system, and proxy-system-status.
# Run proxy-system --help for usage. Sourcing the file does not change settings.
# Commands verified against `networksetup -help` and Apple's documentation:
# https://support.apple.com/guide/remote-desktop/about-networksetup-apdd0c5a2d5/mac

function _mt_macos_check_service() {
    local service="$1" services candidate
    if [[ "${OSTYPE:-}" != darwin* ]]; then
        echo 'System proxy settings require macOS.' >&2
        return 1
    fi
    services=$(networksetup -listallnetworkservices) || return 1
    # The first line explains the asterisk marking disabled services.
    services="${services#*$'\n'}"
    while IFS= read -r candidate; do
        if [[ "$candidate" == "$service" || "$candidate" == "*$service" ]]; then
            return 0
        fi
    done <<<"$services"
    printf 'Unknown network service: %s. Run networksetup -listallnetworkservices.\n' "$service" >&2
    return 1
}

function _mt_macos_networksetup() {
    local result=0
    if [[ $EUID -eq 0 ]]; then
        networksetup "$@" || result=$?
    else
        sudo networksetup "$@" || result=$?
    fi
    if [[ $result -ne 0 ]]; then
        echo 'macOS proxy update failed; system settings may be partially changed.' >&2
    fi
    return "$result"
}

function _mt_macos_proxy() {
    local service="$1" scheme="$2" host="$3" port="$4"
    _mt_macos_check_service "$service" || return 1
    # Setters enable the selected proxy. The final "off" disables authentication.
    case "$scheme" in
        http)
            _mt_macos_networksetup -setwebproxy "$service" "$host" "$port" off || return 1
            _mt_macos_networksetup -setsecurewebproxy "$service" "$host" "$port" off || return 1
            _mt_macos_networksetup -setsocksfirewallproxystate "$service" off || return 1
            ;;
        socks5h)
            _mt_macos_networksetup -setsocksfirewallproxy "$service" "$host" "$port" off || return 1
            _mt_macos_networksetup -setwebproxystate "$service" off || return 1
            _mt_macos_networksetup -setsecurewebproxystate "$service" off || return 1
            ;;
        *)
            printf 'Unsupported system proxy scheme: %s\n' "$scheme" >&2
            return 1
            ;;
    esac
    _mt_macos_networksetup -setautoproxystate "$service" off || return 1
    _mt_macos_networksetup -setproxyautodiscovery "$service" off
}

function _mt_macos_unproxy() {
    local service="$1" option result=0
    _mt_macos_check_service "$service" || return 1
    # Attempt every mode even if an earlier setting fails.
    for option in -setwebproxystate -setsecurewebproxystate \
        -setsocksfirewallproxystate -setautoproxystate -setproxyautodiscovery; do
        _mt_macos_networksetup "$option" "$service" off || result=1
    done
    return "$result"
}

function _mt_macos_proxy_status() {
    local service="${1:-${MT_MACOS_PROXY_SERVICE:-Wi-Fi}}" option result=0
    _mt_macos_check_service "$service" || return 1
    for option in -getwebproxy -getsecurewebproxy -getsocksfirewallproxy \
        -getautoproxyurl -getproxyautodiscovery; do
        printf '%s (%s):\n' "$service" "${option#-get}"
        networksetup "$option" "$service" || result=1
    done
    return "$result"
}

function _mt_macos_usage() {
    cat <<'EOF'
Load the commands with: source /path/to/mt-macos.sh

Usage:
  proxy-system [-p port] [-h host] [-H] [-n service]
  unproxy-system [-n service]
  proxy-system-status [-n service]
  proxy-system --help

Options:
  -p port     Proxy port (default: 2333).
  -h host     Proxy host (default: 127.0.0.1).
  -H          Use HTTP/HTTPS proxies instead of SOCKS.
  -n service  Network service (default: MT_MACOS_PROXY_SERVICE or Wi-Fi).

Examples:
  networksetup -listallnetworkservices
  proxy-system
  proxy-system -H -p 7890 -n "USB Ethernet"
  proxy-system-status -n "USB Ethernet"
  unproxy-system -n "USB Ethernet"

Changes persist after this command exits; sudo may request a password.
Proxy mode disables competing proxy types, PAC, and automatic discovery.
Unproxy disables all proxy modes on the selected service without restoring
earlier settings. Shell proxy environment variables are managed separately.
EOF
}

function _mt_macos_proxy_command() {
    local action="$1" option_spec='n:'
    local host='127.0.0.1' port='2333' scheme='socks5h'
    local service="${MT_MACOS_PROXY_SERVICE:-Wi-Fi}"
    local OPTIND=1 opt
    shift
    if [[ "${1:-}" == --help ]]; then
        _mt_macos_usage
        return 0
    fi
    case "$action" in
        proxy) option_spec='p:h:Hn:' ;;
        unproxy | status) ;;
        *)
            _mt_macos_usage >&2
            return 2
            ;;
    esac
    # -p is for port, -h is for host, -H uses HTTP, -n selects the service.
    while getopts "$option_spec" opt; do
        case "$opt" in
            p) port="$OPTARG" ;;
            h) host="$OPTARG" ;;
            H) scheme='http' ;;
            n) service="$OPTARG" ;;
            *)
                _mt_macos_usage >&2
                return 2
                ;;
        esac
    done
    shift $((OPTIND - 1))
    if [[ $# -ne 0 || -z "$service" ]]; then
        _mt_macos_usage >&2
        return 2
    fi
    if [[ "${OSTYPE:-}" != darwin* ]]; then
        echo 'System proxy settings require macOS.' >&2
        return 1
    fi
    case "$action" in
        proxy)
            case "$port" in
                '' | *[!0-9]*)
                    echo 'Proxy port must be an integer from 1 to 65535.' >&2
                    return 2
                    ;;
            esac
            if [[ ${#port} -gt 5 ]] || ((10#$port < 1 || 10#$port > 65535)); then
                echo 'Proxy port must be an integer from 1 to 65535.' >&2
                return 2
            fi
            port=$((10#$port))
            # networksetup takes bare IPv6 addresses, without URL brackets.
            host="${host#\[}"
            host="${host%\]}"
            if [[ -z "$host" ]]; then
                echo 'Proxy host must not be empty.' >&2
                return 2
            fi
            _mt_macos_proxy "$service" "$scheme" "$host" "$port"
            ;;
        unproxy) _mt_macos_unproxy "$service" ;;
        status) _mt_macos_proxy_status "$service" ;;
    esac
}

function proxy-system() {
    _mt_macos_proxy_command proxy "$@"
}

function unproxy-system() {
    _mt_macos_proxy_command unproxy "$@"
}

function proxy-system-status() {
    _mt_macos_proxy_command status "$@"
}
