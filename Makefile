dev:
	elm-live src/Jots.elm --open

build:
	elm make src/Jots.elm

deploy:
	aws s3 mv index.html s3://jot.tylertracy.com