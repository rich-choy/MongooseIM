# MongooseIM mod_presence_notify Deployment Guide

## Overview

The `mod_presence_notify` module is a custom MongooseIM extension that notifies a designated server user of all presence changes (available/unavailable) for every user on the system. This module is designed for game servers or applications that need centralized presence monitoring.

## Features

- **Unconditional notifications**: Notifies the server user of ALL presence changes, regardless of roster subscriptions
- **Structured JSON payload**: Sends notifications with structured JSON data for easy parsing
- **Environment variable configuration**: Server user can be configured via `WORLD_SERVER_USERNAME` environment variable
- **Production-ready**: Includes proper error handling, logging, and follows MongooseIM best practices
- **Message correlation**: Each notification includes a trace ID that matches the message stanza ID for proper correlation

## Installation

### 1. Copy the Module

Copy the `mod_presence_notify.erl` file to your MongooseIM source directory:

```bash
cp mod_presence_notify.erl /path/to/mongooseim/src/
```

### 2. Compile MongooseIM

Compile MongooseIM with the new module:

```bash
cd /path/to/mongooseim
make rel
```

Or if using rebar3 directly:

```bash
rebar3 compile
rebar3 release
```

### 3. Configuration

#### Enable/Disable the Module

The module is controlled by including it in the `[modules]` section of `mongooseim.toml`:

```toml
# ✅ ENABLE: Add this to your mongooseim.toml
[modules.mod_presence_notify]

# ❌ DISABLE: Remove or comment out the entire section
# [modules.mod_presence_notify]
```

#### Configuration Options

```toml
# Option 1: Use environment variable (RECOMMENDED)
[modules.mod_presence_notify]
# Uses WORLD_SERVER_USERNAME env var, falls back to "server"

# Option 2: Explicit server user
[modules.mod_presence_notify]
  server_user = "world"
```

#### Configuration Precedence

1. **Explicit `server_user` in TOML** (highest priority)
2. **`WORLD_SERVER_USERNAME` environment variable**
3. **Default value "server"** (lowest priority)

### 4. Environment Variables

Set the environment variable before starting MongooseIM:

```bash
export WORLD_SERVER_USERNAME="your_server_user"
```

### 5. Restart MongooseIM

Restart your MongooseIM server to load the new module:

```bash
# Using mongooseimctl
mongooseimctl stop
mongooseimctl start

# Or using systemd
systemctl restart mongooseim
```

## Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `server_user` | binary | `$WORLD_SERVER_USERNAME` or `"server"` | The local part of the JID that will receive presence notifications |

## Message Format

The module sends structured messages to the configured server user with the following format:

```xml
<message type="chat" id="dcf6924a-b0a0-4d50-b685-e37797afa416">
  <body>Presence update</body>
  <event xmlns="flux:xmpp" mime="application/json">
    <![CDATA[
    {
      "trace": "dcf6924a-b0a0-4d50-b685-e37797afa416",
      "type": "PRESENCE_DID_CHANGE",
      "args": {
        "user": "alice@example.com",
        "presence": "available"
      }
    }
    ]]>
  </event>
</message>
```

### JSON Payload Fields

- `trace`: Message stanza ID for tracing and correlation (matches the message `id` attribute)
- `type`: Always `"PRESENCE_DID_CHANGE"`
- `args.user`: Full JID of the user whose presence changed
- `args.presence`: Either `"available"` or `"unavailable"`

## Verification

### 1. Check Module Loading

Verify the module is loaded by checking the logs:

```bash
tail -f /var/log/mongooseim/mongooseim.log | grep presence_notify
```

You should see:
```
[info] what=presence_notify_start, host_type=...
```

### 2. Test Presence Changes

1. Connect a test user to the XMPP server
2. Monitor messages received by your server user
3. Disconnect the test user
4. Verify that both `available` and `unavailable` notifications are received

### 3. Debugging

Enable debug logging in `mongooseim.toml`:

```toml
[general]
  loglevel = "debug"
```

This will show detailed logs for each presence notification sent.

## Production Considerations

### Performance

- The module processes ALL presence changes, so ensure your server user can handle the message volume
- Consider implementing rate limiting or batching in your server user application if needed

### Monitoring

- Monitor the `presence_notify_error` log entries for any issues
- Set up alerting on errors in the presence notification system

### Security

- Ensure the server user JID is properly secured
- Consider limiting access to the server user account
- The module only sends notifications to one designated user, maintaining security

## Troubleshooting

### Common Issues

1. **Module not starting**
   - Check that the module is properly compiled
   - Verify the configuration syntax in `mongooseim.toml`
   - Check MongooseIM logs for compilation errors

2. **No notifications received**
   - Verify the server user exists and is reachable
   - Check that the JID format is correct
   - Enable debug logging to see notification attempts

3. **JSON parsing errors**
   - Ensure `jiffy` is available (it should be included with MongooseIM)
   - Check for any JSON formatting issues in logs

### Log Messages

Key log messages to monitor:

- `presence_notify_start`: Module started successfully
- `presence_notification_sent`: Notification sent (debug level)
- `presence_notify_error`: Error occurred during notification

## Integration Example

Here's a simple example of how to handle these notifications in your server application:

```javascript
// Node.js example using node-xmpp-client
client.on('stanza', (stanza) => {
  if (stanza.is('message')) {
    const event = stanza.getChild('event', 'flux:xmpp');
    if (event) {
      const jsonData = JSON.parse(event.getText());
      if (jsonData.type === 'XMPP_USER_PRESENCE_DID_CHANGE') {
        const { user, presence } = jsonData.args;
        console.log(`User ${user} is now ${presence}`);
        // Handle presence change in your application
        handlePresenceChange(user, presence);
      }
    }
  }
});
```

## Support

For issues or questions:

1. Check the MongooseIM logs for error messages
2. Verify your configuration against this guide
3. Test with a minimal configuration first
4. Enable debug logging for detailed troubleshooting information

## Version Compatibility

This module is compatible with MongooseIM 6.0+ and uses the modern hook system and configuration format.
