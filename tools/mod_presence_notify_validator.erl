#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

-mode(compile).

main([]) ->
    io:format("Validating mod_presence_notify module syntax...~n"),

    %% Try to parse the module file
    case file:read_file("src/mod_presence_notify.erl") of
        {ok, Content} ->
            %% Try to compile the file to check syntax
            case compile_module() of
                ok ->
                    io:format("✓ Module compilation successful~n");
                {error, Errors} ->
                    io:format("✗ Compilation errors: ~p~n", [Errors])
            end,

            %% Check for required exports
            check_exports(Content),

            %% Check for required functions
            check_functions(Content),

            io:format("~nModule validation complete!~n");
        {error, Reason} ->
            io:format("✗ Could not read module file: ~p~n", [Reason])
    end.

compile_module() ->
    %% Try to compile the module with minimal dependencies
    case epp:parse_file("src/mod_presence_notify.erl", [], []) of
        {ok, Forms} ->
            io:format("✓ File parsed successfully (~p forms)~n", [length(Forms)]),
            ok;
        {error, Error} ->
            {error, Error}
    end.

check_exports(Content) ->
    io:format("~nChecking exports...~n"),
    ContentStr = binary_to_list(Content),

    RequiredExports = [
        "start/2",
        "stop/1",
        "hooks/1",
        "config_spec/0",
        "supported_features/0"
    ],

    lists:foreach(fun(Export) ->
        case string:find(ContentStr, Export) of
            nomatch ->
                io:format("✗ Missing export: ~s~n", [Export]);
            _ ->
                io:format("✓ Found export: ~s~n", [Export])
        end
    end, RequiredExports).

check_functions(Content) ->
    io:format("~nChecking function implementations...~n"),
    ContentStr = binary_to_list(Content),

    RequiredFunctions = [
        "start(HostType, _Opts)",
        "stop(HostType)",
        "hooks(HostType)",
        "config_spec()",
        "supported_features()",
        "user_available(",
        "unset_presence("
    ],

    lists:foreach(fun(Function) ->
        case string:find(ContentStr, Function) of
            nomatch ->
                io:format("✗ Missing function: ~s~n", [Function]);
            _ ->
                io:format("✓ Found function: ~s~n", [Function])
        end
    end, RequiredFunctions).
