# Overview

Parallel HTTP Get example.

    time go run main.go -threads=5 -requests=100  https://your-own-url-here

    go compile main.go
    time go ./main -threads=5 -requests=100  https://your-own-url-here

This example could serve as a starting point for a monitoring tool, a benchmarking tool, etc.
