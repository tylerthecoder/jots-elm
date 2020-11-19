dev:
	elm-live src/Jots.elm --open -- --output=elm.js

build:
	elm make src/Jots.elm --output=elm.js

package:
	mkdir dist
	cp index.html dist/index.html
	cp style.css dist/style.css
	cp elm.js dist/elm.js

deploy:
	aws s3 mv dist s3://jot.tylertracy.com --recursive

clean:
	rm -rf dist