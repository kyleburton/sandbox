lsb_release -a
echo "bash version:    $BASH_VERSION"
echo "ffprobe version: $(ffprobe -version | head -n1)"
echo "sed version:     $(sed --version | head -n1)"
