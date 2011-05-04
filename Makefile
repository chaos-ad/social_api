all: compile

compile: 
	erl -pa "./ebin" -make

clean:
	rm ebin/*.beam -rf
	rm thrift/gen-erl/* -rf
