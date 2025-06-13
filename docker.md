# MongooseIM with mod_presence_notify - Docker Deployment

This directory contains Docker configuration for running MongooseIM with the custom `mod_presence_notify` module.

## Quick Start

### 1. Build the Docker Image

```bash
# Build the image with our custom module
docker build -t industrydigital/mongooseim-presence-notify:latest .
```

### 2. Run with Docker Compose

```bash
# Start MongooseIM only
docker-compose -f docker-compose.mongooseim.yml up -d

# Start with PostgreSQL database
docker-compose -f docker-compose.mongooseim.yml --profile database up -d
```

### 3. Run with Docker

```bash
# Simple run
docker run -d \
  --name mongooseim-presence-notify \
  -p 5222:5222 \
  -p 5280:5280 \
  -e WORLD_SERVER_USERNAME=server \
  industrydigital/mongooseim-presence-notify:latest

# With custom configuration
docker run -d \
  --name mongooseim-presence-notify \
  -p 5222:5222 \
  -p 5280:5280 \
  -v $(pwd)/docker/mongooseim.toml:/opt/mongooseim/etc/mongooseim.toml:ro \
  -e WORLD_SERVER_USERNAME=server \
  industrydigital/mongooseim-presence-notify:latest
```

## Configuration

### Environment Variables

- `WORLD_SERVER_USERNAME`: Username for the server user that receives presence notifications (default: "server")
- `MONGOOSEIM_CONFIG_FORMAT`: Configuration format (default: "toml")
- `MONGOOSEIM_CONFIG_PATH`: Path to configuration file (default: "/opt/mongooseim/etc/mongooseim.toml")

### Module Configuration

The `mod_presence_notify` module can be configured in your `mongooseim.toml`:

```toml
[modules.mod_presence_notify]
  # Use environment variable (recommended)
  # server_username will be read from WORLD_SERVER_USERNAME env var

  # Or specify directly
  # server_username = "server"
```

Example configuration is available at `/opt/mongooseim/examples/mod_presence_notify.toml` inside the container.

## Ports

- **5222**: XMPP Client connections (C2S)
- **5269**: XMPP Server-to-Server (S2S)
- **5280**: HTTP/REST API and WebSocket
- **9100**: Metrics endpoint

## Health Check

The container includes a health check that verifies MongooseIM is responding:

```bash
curl -f http://localhost:5280/api/health
```

## Volumes

- `/var/lib/mongooseim`: Data directory
- `/var/log/mongooseim`: Log files
- `/opt/mongooseim/etc/mongooseim.toml`: Main configuration file

## Technical Details

### Erlang Version Compatibility

This Docker image uses **Erlang/OTP 26.2.5** instead of the newer 28.0 due to compatibility issues with the `rabbit_common` dependency (version 4.0.3) used by MongooseIM.

The `rabbit_common` 4.0.3 library has known compilation issues with Erlang 28.0, specifically:
- Undefined macros like `'id-ce-subjectAltName'` and `'street-address'`
- Missing public key includes and records

Erlang 26.2.5 is:
- ✅ Compatible with MongooseIM's minimum requirement (OTP 26+)
- ✅ Stable and well-tested with MongooseIM
- ✅ Compatible with `rabbit_common` 4.0.3
- ✅ Used by the official MongooseIM Docker images

### Build Process

The Docker build:
1. Uses multi-stage build for smaller final image
2. Compiles MongooseIM with our custom module
3. Verifies the module was compiled successfully
4. Creates a runtime image with only necessary components
5. Sets up proper user permissions and directories

### Module Verification

The build process includes verification steps:
- Checks that `src/mod_presence_notify.erl` exists before building
- Verifies that `mod_presence_notify.beam` was compiled into the release
- Copies configuration examples to the runtime image

## Troubleshooting

### Build Issues

If you encounter build issues:

1. **Erlang Version**: Ensure you're using Erlang 26.x, not 27.x or 28.x
2. **Dependencies**: The build downloads all dependencies automatically
3. **Module Files**: Ensure all module files are present in the correct directories

### Runtime Issues

1. **Check logs**: `docker logs mongooseim-presence-notify`
2. **Health check**: `curl http://localhost:5280/api/health`
3. **Module status**: Check if the module is loaded in MongooseIM admin interface

### Common Errors

- **rabbit_common compilation errors**: Use Erlang 26.x instead of newer versions
- **Module not found**: Ensure `mod_presence_notify.erl` is in the `src/` directory
- **Permission denied**: The container runs as user `mongooseim`, ensure volume permissions are correct

## Development

### Rebuilding After Changes

```bash
# Rebuild the image
docker build -t industrydigital/mongooseim-presence-notify:latest .

# Restart the container
docker-compose -f docker-compose.mongooseim.yml down
docker-compose -f docker-compose.mongooseim.yml up -d
```

### Testing the Module

1. Start the container
2. Connect an XMPP client
3. Change presence status
4. Check that the server user receives notifications

## Production Deployment

For production use:

1. **Use specific tags**: Tag your images with versions instead of `latest`
2. **External database**: Use PostgreSQL or other external database
3. **Persistent volumes**: Ensure data and logs are persisted
4. **Monitoring**: Set up monitoring for the metrics endpoint (port 9100)
5. **Security**: Configure TLS/SSL certificates
6. **Clustering**: For high availability, set up MongooseIM clustering

## Support

- MongooseIM Documentation: https://mongooseim.readthedocs.io/
- Docker Hub Erlang Images: https://hub.docker.com/_/erlang
- Module Source: See `src/mod_presence_notify.erl` and `test/mod_presence_notify_SUITE.erl`
