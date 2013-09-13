#!/bin/sh
find . -type f -name "*.py" -print0 | xargs -0 sed -i '' -E "s/[[:space:]]*$//"

