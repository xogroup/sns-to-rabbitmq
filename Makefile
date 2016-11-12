NAME=xolocalvendors/http-to-rabbitmq
VERSION=$$(git rev-parse --short HEAD)

# clean:
# 	@rm -f npm-shrinkwrap.json
# 	make install
# 	shonkwrap

# install:
# 	@rm -rf ./node_modules
# 	npm install

docker-build:
	mkdir -p bin
	docker run --rm -v $(PWD):/home/haskell/app -v $(HOME)/.stack:/home/haskell/.stack -w /home/haskell/app xolocalvendors/haskellbox:latest sh -c 'stack clean  && stack --system-ghc build --ghc-options " -threaded -with-rtsopts=-N" && stack install && mv /home/haskell/.local/bin/* /home/haskell/app/bin/'
	docker build -t $(NAME) -f docker/Dockerfile .


docker-run: docker-build
	docker run -d --rm -p 3000:3000 $(NAME)

jenkins-build: docker-build
	docker tag -f $(NAME) $(NAME):$(VERSION)
	docker tag -f $(NAME) $(NAME):latest 2>/dev/null

jenkins-push:
	docker push $(NAME):$(VERSION)

jenkins-clean:
	docker rmi -f $(NAME)

aws-build:
	cat docker/Dockerrun.aws.json.template | sed "s/{version}/$(VERSION)/" > deployment/Dockerrun.aws.json

# .PHONY: clean install docker-build docker-run jenkins-build jenkins-push jenkins-clean aws-build
.PHONY: docker-build docker-run jenkins-build jenkins-push jenkins-clean aws-build
