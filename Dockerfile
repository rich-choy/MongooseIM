# syntax=docker/dockerfile:1
# MongooseIM with mod_presence_notify custom module
# Based on official Erlang images: https://hub.docker.com/_/erlang

ARG ERLANG_VERSION=26.2.5
ARG DEBIAN_VERSION=bookworm

# Build stage
FROM erlang:${ERLANG_VERSION}-slim AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    git \
    make \
    cmake \
    zlib1g-dev \
    libssl-dev \
    unixodbc-dev \
    libexpat1-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /opt/mongooseim

# Copy source code (respects .dockerignore)
COPY . .

# Ensure our custom module is present
RUN test -f src/mod_presence_notify.erl || (echo "ERROR: mod_presence_notify.erl not found!" && exit 1)

# Configure and build MongooseIM
RUN ./tools/configure \
    && make rel

# Verify our module was compiled
RUN test -f _build/prod/rel/mongooseim/lib/mongooseim-*/ebin/mod_presence_notify.beam || \
    (echo "ERROR: mod_presence_notify.beam not found in release!" && exit 1)

# Runtime stage
FROM debian:${DEBIAN_VERSION}-slim AS runtime

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    openssl \
    unixodbc \
    procps \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create mongooseim user
RUN groupadd -r mongooseim && useradd -r -g mongooseim mongooseim

# Copy MongooseIM release from builder
COPY --from=builder /opt/mongooseim/_build/prod/rel/mongooseim /opt/mongooseim
COPY --from=builder /opt/mongooseim/priv/examples/mod_presence_notify.toml /opt/mongooseim/examples/

# Set ownership
RUN chown -R mongooseim:mongooseim /opt/mongooseim

# Create directories for logs and data
RUN mkdir -p /var/log/mongooseim /var/lib/mongooseim \
    && chown -R mongooseim:mongooseim /var/log/mongooseim /var/lib/mongooseim

# Switch to mongooseim user
USER mongooseim

# Set working directory
WORKDIR /opt/mongooseim

# Environment variables
ENV MONGOOSEIM_CONFIG_FORMAT=toml
ENV MONGOOSEIM_CONFIG_PATH=/opt/mongooseim/etc/mongooseim.toml
ENV WORLD_SERVER_USERNAME=server

# Expose ports
EXPOSE 5222 5280 5269 8080 9100

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:5280/api/health || exit 1

# Default command
CMD ["./bin/mongooseim", "foreground"]

# Labels
LABEL maintainer="Industry Digital PTE Ltd <eng@industrydigital.dev>"
LABEL description="MongooseIM XMPP server with mod_presence_notify custom module"
LABEL version="6.3.3-custom"
LABEL org.opencontainers.image.source="https://github.com/esl/MongooseIM"
LABEL org.opencontainers.image.documentation="https://mongooseim.readthedocs.io/"
LABEL org.opencontainers.image.vendor="Industry Digital PTE Ltd"
LABEL org.opencontainers.image.title="MongooseIM with mod_presence_notify"
LABEL org.opencontainers.image.description="Production-ready MongooseIM server with custom presence notification module"
