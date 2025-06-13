# MongooseIM Docker Deployment with mod_presence_notify

This directory contains Docker configuration files for running MongooseIM with the custom `mod_presence_notify` module.

## Quick Start

### 1. Build and Run with Docker Compose

```bash
# Build and start MongooseIM
docker-compose up -d

# View logs
docker-compose logs -f mongooseim

# Stop the server
docker-compose down
```

### 2. Build Docker Image Only

```bash
# Build the image
docker build -t mongooseim-presence-notify .

# Run the container
docker run -d \
  --name mongooseim \
  -p 5222:5222 \
  -p 5280:5280 \
  -e WORLD_SERVER_USERNAME=world_server \
  mongooseim-presence-notify
```

## Configuration

### Environment Variables

- `WORLD_SERVER_USERNAME`: Username for the server user that receives presence notifications (default: "server")
- `MONGOOSEIM_CONFIG_FORMAT`: Configuration format, should be "toml" (default: "toml")
- `MONGOOSEIM_CONFIG_PATH`: Path to configuration file (default: "/opt/mongooseim/etc/mongooseim.toml")

### Custom Configuration

To use a custom configuration file:

```bash
# Mount your config file
docker run -d \
  --name mongooseim \
  -p 5222:5222 \
  -p 5280:5280 \
  -v /path/to/your/mongooseim.toml:/opt/mongooseim/etc/mongooseim.toml:ro \
  mongooseim-presence-notify
```

## Ports

- **5222**: XMPP Client-to-Server (C2S)
- **5280**: HTTP (BOSH, WebSocket, Admin API)
- **5269**: XMPP Server-to-Server (S2S)
- **9100**: Metrics endpoint (if enabled)

## Database Options

### Development (Default)
Uses Mnesia (embedded database) - suitable for development and testing.

### Production with PostgreSQL
```bash
# Start with PostgreSQL
docker-compose --profile database up -d

# Or with database management UI
docker-compose --profile database --profile admin up -d
```

## Testing the Module

### 1. Create Test Users

```bash
# Connect to the running container
docker exec -it mongooseim-presence-notify /opt/mongooseim/bin/mongooseimctl

# Register users
mongooseimctl register alice localhost password123
mongooseimctl register world_server localhost password123
```

### 2. Test Presence Notifications

Connect two XMPP clients:
1. **world_server@localhost** - Will receive presence notifications
2. **alice@localhost** - Test user whose presence changes will be monitored

When alice comes online/offline, world_server will receive messages like:

```xml
<message type='chat' id='msg-1234567890'>
  <body>Presence update</body>
  <event xmlns='flux:xmpp' mime='application/json'>
    <![CDATA[
    {
      "trace": "msg-1234567890",
      "type": "PRESENCE_DID_CHANGE",
      "args": {
        "user": "alice@localhost/resource",
        "presence": "available"
      }
    }
    ]]>
  </event>
</message>
```

## Health Checks

The container includes a health check that verifies MongooseIM is responding:

```bash
# Check container health
docker ps

# Manual health check
curl http://localhost:5280/api/health
```

## Logs

### View Logs
```bash
# Docker Compose
docker-compose logs -f mongooseim

# Docker
docker logs -f mongooseim
```

### Log Locations in Container
- Application logs: `/var/log/mongooseim/`
- Console output: Docker logs

## Troubleshooting

### Module Not Loading
1. Check that `mod_presence_notify.beam` exists in the release:
   ```bash
   docker exec mongooseim-presence-notify find /opt/mongooseim -name "mod_presence_notify.beam"
   ```

2. Verify module configuration in `mongooseim.toml`:
   ```toml
   [modules.mod_presence_notify]
   ```

3. Check logs for module startup messages:
   ```bash
   docker-compose logs mongooseim | grep presence_notify
   ```

### Connection Issues
1. Verify ports are exposed:
   ```bash
   docker port mongooseim-presence-notify
   ```

2. Test XMPP connection:
   ```bash
   telnet localhost 5222
   ```

3. Test HTTP endpoint:
   ```bash
   curl http://localhost:5280/api/health
   ```

### Environment Variables
Check that environment variables are set correctly:
```bash
docker exec mongooseim-presence-notify env | grep WORLD_SERVER
```

## Production Deployment

### Security Considerations
1. **Change default passwords** in production
2. **Use TLS certificates** for XMPP connections
3. **Configure proper access controls** in mongooseim.toml
4. **Use external database** (PostgreSQL) for persistence
5. **Set up monitoring** and log aggregation

### Example Production Configuration
```yaml
# docker-compose.prod.yml
version: '3.8'
services:
  mongooseim:
    image: mongooseim-presence-notify:latest
    environment:
      - WORLD_SERVER_USERNAME=production_server
    volumes:
      - ./config/mongooseim.prod.toml:/opt/mongooseim/etc/mongooseim.toml:ro
      - ./certs:/opt/mongooseim/priv/ssl:ro
    networks:
      - production_network
    restart: always
```

## Development

### Building Custom Images
```bash
# Build with specific Erlang version
docker build --build-arg ERLANG_VERSION=27.0 -t mongooseim-presence-notify:erlang27 .

# Build with different base image
docker build --build-arg DEBIAN_VERSION=bullseye -t mongooseim-presence-notify:bullseye .
```

### Running Tests
```bash
# Run validation tools
docker run --rm mongooseim-presence-notify escript tools/mod_presence_notify_validator.erl

# Run functional tests
docker run --rm mongooseim-presence-notify escript tools/mod_presence_notify_functional_test.erl
```

## Support

For issues related to:
- **mod_presence_notify module**: Check the module documentation in `doc/modules/mod_presence_notify.md`
- **MongooseIM core**: See [MongooseIM documentation](https://mongooseim.readthedocs.io/)
- **Docker deployment**: Review this README and Docker logs
