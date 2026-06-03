#!/bin/zsh

# Double-click this file on macOS to rebuild Medium image download queues and
# download missing thumbnail/body images. Reusable logic lives in scripts/.

cd "$(dirname "$0")/../.." || exit 1
exec zsh scripts/manual_tools/download_missing_medium_images.zsh "$@"
