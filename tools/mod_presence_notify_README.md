# mod_presence_notify Development Tools

This directory contains development and testing tools for the `mod_presence_notify` module.

## Tools Overview

### mod_presence_notify_validator.erl
**Purpose**: Validates the syntax and structure of the mod_presence_notify module.

**Usage**:
```bash
escript tools/mod_presence_notify_validator.erl
```

**What it checks**:
- Erlang syntax validation
- Required exports (start/2, stop/1, hooks/1, etc.)
- Required function implementations
- Module structure compliance

### mod_presence_notify_functional_test.erl
**Purpose**: Runs functional tests of the module's core logic without MongooseIM dependencies.

**Usage**:
```bash
escript tools/mod_presence_notify_functional_test.erl
```

**What it tests**:
- Environment variable handling
- Message creation and structure
- JSON payload format
- Trace ID correlation
- Configuration logic

### mod_presence_notify_demo.erl
**Purpose**: Demonstrates how the module works in a simulated MongooseIM environment.

**Usage**:
```bash
escript tools/mod_presence_notify_demo.erl
```

**What it shows**:
- Module startup simulation
- Presence change event handling
- Configuration examples
- Message structure examples
- Integration workflow

## Running All Tools

To run all validation and testing tools:

```bash
# Validate syntax
escript tools/mod_presence_notify_validator.erl

# Run functional tests
escript tools/mod_presence_notify_functional_test.erl

# View integration demo
escript tools/mod_presence_notify_demo.erl
```

## Development Workflow

1. **After making changes to the module**: Run the validator to check syntax
2. **Before committing**: Run functional tests to ensure logic is correct
3. **For demonstration**: Use the demo script to show functionality

## Integration with MongooseIM Testing

These tools complement the official MongooseIM testing framework:

- **Unit tests**: Use `test/mod_presence_notify_SUITE.erl` with `./rebar3 eunit`
- **Integration tests**: Use MongooseIM's big_tests framework
- **Development tools**: Use these standalone tools for quick validation

## Requirements

- Erlang/OTP (same version as MongooseIM)
- No additional dependencies (tools use mocking for external dependencies)
