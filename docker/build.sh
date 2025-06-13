#!/bin/bash
set -e

# MongooseIM Docker Build Script
# Builds MongooseIM with mod_presence_notify module

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Default values
IMAGE_NAME="mongooseim-presence-notify"
TAG="latest"
ERLANG_VERSION="28.0"
DEBIAN_VERSION="bookworm"
BUILD_ARGS=""
PUSH=false
TEST=false

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -n, --name NAME          Image name (default: $IMAGE_NAME)"
    echo "  -t, --tag TAG            Image tag (default: $TAG)"
    echo "  -e, --erlang VERSION     Erlang version (default: $ERLANG_VERSION)"
    echo "  -d, --debian VERSION     Debian version (default: $DEBIAN_VERSION)"
    echo "  -p, --push               Push image to registry after build"
    echo "  --test                   Run tests after build"
    echo "  -h, --help               Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                                    # Build with defaults"
    echo "  $0 -n myregistry/mongooseim -t v1.0  # Custom name and tag"
    echo "  $0 -e 27.0 --test                    # Use Erlang 27.0 and run tests"
    echo "  $0 --push                            # Build and push to registry"
}

log() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
    exit 1
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -n|--name)
            IMAGE_NAME="$2"
            shift 2
            ;;
        -t|--tag)
            TAG="$2"
            shift 2
            ;;
        -e|--erlang)
            ERLANG_VERSION="$2"
            shift 2
            ;;
        -d|--debian)
            DEBIAN_VERSION="$2"
            shift 2
            ;;
        -p|--push)
            PUSH=true
            shift
            ;;
        --test)
            TEST=true
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            error "Unknown option: $1"
            ;;
    esac
done

FULL_IMAGE_NAME="${IMAGE_NAME}:${TAG}"

log "Building MongooseIM Docker image with mod_presence_notify"
log "Image: $FULL_IMAGE_NAME"
log "Erlang version: $ERLANG_VERSION"
log "Debian version: $DEBIAN_VERSION"

# Change to project root
cd "$PROJECT_ROOT"

# Verify required files exist
if [[ ! -f "src/mod_presence_notify.erl" ]]; then
    error "mod_presence_notify.erl not found in src/ directory"
fi

if [[ ! -f "Dockerfile" ]]; then
    error "Dockerfile not found in project root"
fi

# Build the image
log "Building Docker image..."
docker build \
    --build-arg ERLANG_VERSION="$ERLANG_VERSION" \
    --build-arg DEBIAN_VERSION="$DEBIAN_VERSION" \
    -t "$FULL_IMAGE_NAME" \
    .

if [[ $? -eq 0 ]]; then
    log "Docker image built successfully: $FULL_IMAGE_NAME"
else
    error "Docker build failed"
fi

# Run tests if requested
if [[ "$TEST" == "true" ]]; then
    log "Running tests..."

    # Test that the module was compiled
    log "Checking if mod_presence_notify.beam exists in image..."
    docker run --rm "$FULL_IMAGE_NAME" find /opt/mongooseim -name "mod_presence_notify.beam" | grep -q "mod_presence_notify.beam"
    if [[ $? -eq 0 ]]; then
        log "✓ mod_presence_notify.beam found in image"
    else
        error "✗ mod_presence_notify.beam not found in image"
    fi

    # Test that MongooseIM starts
    log "Testing MongooseIM startup..."
    CONTAINER_ID=$(docker run -d "$FULL_IMAGE_NAME")
    sleep 10

    # Check if container is still running
    if docker ps -q --no-trunc | grep -q "$CONTAINER_ID"; then
        log "✓ MongooseIM started successfully"
        docker stop "$CONTAINER_ID" > /dev/null
    else
        error "✗ MongooseIM failed to start"
    fi

    docker rm "$CONTAINER_ID" > /dev/null

    log "All tests passed!"
fi

# Push to registry if requested
if [[ "$PUSH" == "true" ]]; then
    log "Pushing image to registry..."
    docker push "$FULL_IMAGE_NAME"
    if [[ $? -eq 0 ]]; then
        log "Image pushed successfully: $FULL_IMAGE_NAME"
    else
        error "Failed to push image"
    fi
fi

# Show image info
log "Build complete!"
echo ""
echo "Image: $FULL_IMAGE_NAME"
echo "Size: $(docker images --format "table {{.Size}}" "$FULL_IMAGE_NAME" | tail -n 1)"
echo ""
echo "To run the container:"
echo "  docker run -d -p 5222:5222 -p 5280:5280 --name mongooseim $FULL_IMAGE_NAME"
echo ""
echo "To run with Docker Compose:"
echo "  docker-compose up -d"
