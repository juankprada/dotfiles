if functions -q kerl_deactivate
    kerl_deactivate
end

set _KERL (command -v kerl)

if test -n "$_KERL"
  set _KERL_VERSION ($_KERL version)
end

if test -n "$_KERL_VERSION" -a "$_KERL_VERSION" != "4.2.0"
  echo "WARNING: this Erlang/OTP installation appears to be stale. Please consider reinstalling."
  echo "         It was created with kerl 4.2.0, and the current version is $_KERL_VERSION."
end

set -e _KERL_VERSION _KERL

set -x _KERL_ACTIVE_DIR "/home/juankprada/.local/bin/erlang/27.0.1"
set -p _KERL_CLEANUP "set -e _KERL_ACTIVE_DIR;"

function _kerl_remove_el_ --description 'remove elements from array'
    set path_var $argv[1]
    set elements $argv[2]
    echo "
        for el in \$$elements;
            if set -l index (contains -i -- \$el \$$path_var);
                set -e $path_var[1][\$index];
            end;
        end;
    "
end

set -x _KERL_PATH_REMOVABLE "/home/juankprada/.local/bin/erlang/27.0.1/bin"
set -l _KERL_ERL_CALL_REMOVABLE (find "/home/juankprada/.local/bin/erlang/27.0.1" -type d -path "*erl_interface*/bin" 2>/dev/null)
if test -n "$_KERL_ERL_CALL_REMOVABLE"
    set -a _KERL_PATH_REMOVABLE $_KERL_ERL_CALL_REMOVABLE
end
set -p _KERL_CLEANUP "set -e _KERL_PATH_REMOVABLE;"
set -xp PATH $_KERL_PATH_REMOVABLE
set -p _KERL_CLEANUP (_kerl_remove_el_ PATH _KERL_PATH_REMOVABLE)

set -x _KERL_MANPATH_REMOVABLE "/home/juankprada/.local/bin/erlang/27.0.1/lib/erlang/man" "/home/juankprada/.local/bin/erlang/27.0.1/man"
set -p _KERL_CLEANUP "set -e _KERL_MANPATH_REMOVABLE;"
if set -q MANPATH
    if test -n "$MANPATH"
        set -xp MANPATH $_KERL_MANPATH_REMOVABLE
    else
        set -x MANPATH $_KERL_MANPATH_REMOVABLE
    end
else
    set -x MANPATH $_KERL_MANPATH_REMOVABLE
    set -p _KERL_CLEANUP "
        if test -z \"\$MANPATH\"
            set -e MANPATH
        end
    "
end
set -p _KERL_CLEANUP (_kerl_remove_el_ MANPATH _KERL_MANPATH_REMOVABLE)

functions -e _kerl_remove_el_

if set -q REBAR_PLT_DIR
    set -p _KERL_CLEANUP "set -x REBAR_PLT_DIR \"$REBAR_PLT_DIR\";"
else
    set -p _KERL_CLEANUP "set -e REBAR_PLT_DIR;"
end
set -x REBAR_PLT_DIR "/home/juankprada/.local/bin/erlang/27.0.1"

if set -q REBAR_CACHE_DIR
    set -p _KERL_CLEANUP "set -x REBAR_CACHE_DIR \"$REBAR_CACHE_DIR\";"
else
    set -p _KERL_CLEANUP "set -e REBAR_CACHE_DIR;"
end
set -x REBAR_CACHE_DIR "/home/juankprada/.local/bin/erlang/27.0.1/.cache/rebar3"

if test -f "/home/juankprada/.kerlrc.fish"
    source "/home/juankprada/.kerlrc.fish"
end
if set --query KERL_ENABLE_PROMPT
    if functions -q fish_prompt
        functions --copy fish_prompt _kerl_saved_prompt
        set -p _KERL_CLEANUP "
            functions --copy _kerl_saved_prompt fish_prompt
            functions --erase _kerl_saved_prompt;
        "
    end
    function fish_prompt
        printf "%b" "(27.0.1)"
        _kerl_saved_prompt
    end
    set -p _KERL_CLEANUP "functions --erase fish_prompt;"
end

eval "function kerl_deactivate -S --description \"deactivate erlang environment\"
    $_KERL_CLEANUP
    functions -e kerl_deactivate
end"
set -e _KERL_CLEANUP
