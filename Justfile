TAG := "latest"
remote := "192.168.1.244"

ghcid:
    ghcid \
    	--command "stack ghci \
    		--test \
    		--bench \
    		--main-is twenty48:exe:twenty48"

ghcid-yesod:
    ghcid \
    	--command "stack ghci twenty48 \
    		--flag twenty48:dev \
    		--test --bench \
    		--main-is :twenty48 \
    		--ghci-options=-fobject-code" \
    	--test DevelMain.update \
    	--warnings \
    	--reload static \
    	--reload templates

ghcid-test:
    ghcid \
    	--command "stack ghci \
    		--test \
    		--bench \
    		--main-is :test \
    		--ghci-options=-fobject-code" \
    	--test main \
    	--warnings

docker-build:
    docker build -t "dfacastro/base" "./docker/base"
    stack --stack-yaml "./stack-docker.yaml" image container
    docker image tag "dfacastro/2048-twenty48:latest" "dfacastro/2048-twenty48:{{ TAG }}"

docker-publish:
    docker push "dfacastro/2048-twenty48"

nix-build:
    nix build .#twenty48

# Deploy to the Raspberry Pi
rpi-deploy:
    #!/usr/bin/env bash
    set -euxo pipefail
    # Use a shebang to be able to set variables
    # See: https://just.systems/man/en/setting-variables-in-a-recipe.html

    BUNDLE_PATH=$(nix build .#twenty48-rpi --print-out-paths --no-link); \
    echo "$BUNDLE_PATH"; \
    nix copy "$BUNDLE_PATH" --to "ssh://{{ remote }}"; \
    ssh {{ remote }} -- "\
        nix profile remove twenty48-exe-twenty48-aarch64-unknown-linux-gnu; \
        nix profile add $BUNDLE_PATH; \
        "

# Setup the Raspberry Pi
[confirm]
rpi-setup client_session_key_file:
    ssh {{ remote }} -- mkdir -p /home/dc/.local/share/twenty48
    scp {{ client_session_key_file }} dc@{{ remote }}:/home/dc/.local/share/twenty48/client_session_key.aes
