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

TAG = latest
docker-build:
	docker build -t "dfacastro/base" "./docker/base" && \
	stack --stack-yaml "./stack-docker.yaml" image container && \
	docker image tag dfacastro/2048-twenty48:latest dfacastro/2048-twenty48:$(TAG)

docker-publish:
	docker push dfacastro/2048-twenty48

nix-build:
	nix build .#twenty48

# Address of the Raspberry Pi machine
remote := "192.168.1.244"

rpi-deploy:
	BUNDLE_PATH=$$(nix build .#twenty48-rpi --print-out-paths --no-link) ; \
	echo $$BUNDLE_PATH ;\
	nix copy $$BUNDLE_PATH --to ssh://$(remote) ;\
	ssh $(remote) -- "\
		nix profile remove twenty48-rpi ;\
		nix profile add $$BUNDLE_PATH ;\
		"
