default:
	echo "-define(VERSION,\"`git rev-parse HEAD | head -c 6`\")." > include/om.hrl
	./mad dep com pla bun om
