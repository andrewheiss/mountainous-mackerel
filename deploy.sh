REMOTE_HOST="cloud"
REMOTE_DIR="~/sites/stats/public_html/mountainous-mackerel"
REMOTE_DEST=$REMOTE_HOST:$REMOTE_DIR

echo "Uploading new changes to remote server..."
echo
rsync -crvP --exclude '*_cache' --delete _site/ $REMOTE_DEST
