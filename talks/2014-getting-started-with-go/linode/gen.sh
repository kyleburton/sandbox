test -e api.spec.json || curl "https://api.linode.com/?api_action=api.spec" > api.spec.json
