#!/bin/sh
#

prog=$0

swankport=disable
framework=carbon
application_class="BOSCO-CARBON-APPLICATION"
application_name="Bosco"
bundle_path="./bin/${application_name}.app"
system_definition="./bosco.asd"

# usage <exit status>
#
# if exit status is 0, usage will print usage to standard out.  if exit
# status is not 0, usage will print usage to standard error.  usage exits
# with exit status.
usage() {
    local status="$1"

    if [ "$status" -eq 0 ]; then
        cat
    else
        cat >&2
    fi << EOF
Usage: $prog [-h] [-p PORT] [-f FRAMEWORK] [-c APPLICATION_CLASS]  [-b BUNDLE_PATH]
             [-a APPLICATION_NAME] [-s SYSTEM_DEFINITION] [-m MAKE_FORM]
       -h print this help message
       -p PORT [default=disable] the port on which the application listens for
          SLIME connections
       -f FRAMEWORK [default="carbon"] the framework library used to build the application
       -c APPLICATION_CLASS [default="BOSCO-CARBON-APPLICATION"] the CLOS class that
          implements the application. The class must support the framework chosen with
          -f. 
       -b BUNDLE_PATH [default="./bin/Bosco.app"] the application bundle in which
          the application will be built
       -a APPLICATION_NAME [default="Bosco"] the name used for the Lisp kernel and image
       -s SYSTEM_DEFINITION [default="./bosco.asd"] the system definition file to 
          load before building
EOF
    exit $status
}


while getopts "p:f:c:a:s:m:b:h" opt ; do
    case $opt in
		# -p swankport (the port on which the app accepts slime connections)
    	p) swankport=$OPTARG ;;
		# -f framework (either carbon or cocoa)
    	f) framework=$OPTARG ;;
		# -c application-class (either one of "BOSCO-CARBON-APPLICATION",
		#                       "BOSCO-COCOA-APPLICATION", or a user-defined 
		#                       class. The class must be one that works correctly
		#                       with the selected framework.)
    	c) application_class=$OPTARG ;;
    	a) application_name=$OPTARG ;;
    	s) system_definition=$OPTARG ;;
    	b) bundle_path=$OPTARG ;;
    	h) usage 0 ;;
    esac
done

echo
echo
echo "Building application with framework ${framework}"
echo "  with application-class \"${application_class}\""
echo "Start the slime connection to this Bosco build on port ${swankport}"
echo

make_form="(make :swank-port '${swankport} :framework :${framework} :application-class \"${application_class}\" :application-name \"${application_name}\")"

echo 
echo "${make_form}"
echo

openmcl -l ${system_definition} -e "${make_form}"
mv ${application_name} ${bundle_path}/Contents/MacOS
mv ${application_name}.image ${bundle_path}/Contents/MacOS
touch ${bundle_path}

