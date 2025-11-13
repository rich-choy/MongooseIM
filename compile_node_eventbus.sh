#!/bin/bash
set -e

echo "Compiling node_eventbus.erl with minimal dependencies..."

# Set ERL_LIBS so -include_lib() directives can find dependencies
export ERL_LIBS="_build/default/lib:_build/test/lib"

# Build code path
CODE_PATH=""
for dir in _build/default/lib/*/ebin _build/test/lib/*/ebin; do
    if [ -d "$dir" ]; then
        CODE_PATH="$CODE_PATH -pa $dir"
    fi
done

# Compile node_eventbus with all necessary include paths
erlc \
    -I include \
    -I src/pubsub \
    $CODE_PATH \
    -W0 \
    -o /tmp \
    src/pubsub/node_eventbus.erl

if [ $? -eq 0 ]; then
    echo "✓ node_eventbus.beam compiled successfully to /tmp"
    ls -lh /tmp/node_eventbus.beam
else
    echo "✗ Compilation failed"
    exit 1
fi
