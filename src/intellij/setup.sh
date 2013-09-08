#!/usr/bin/env bash
#
# Generates IntelliJ IDEA project files based on the checked-in samples.
#

set -e
export SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
export BASE="$( cd "$( dirname "$0" )"/../.. && pwd )"
echo "About to delete .ipr and .iml files and replace with the .SAMPLE files. Press enter to continue or CTRL-C to cancel."
read

(rm *.ipr *.iml 2>/dev/null)
for f in $(ls "$SCRIPT_DIR"/*.SAMPLE); do
	NEW_FILE=`echo $f | perl -pe 's/.SAMPLE//'`;

	cp $f $NEW_FILE

	# IntelliJ doesn't process the "compilerOptions" setting for variable
	# replacement. If it did, we would just use "$PROJECT_DIR$". Instead,
	# we do this replacement ourselves.
	perl -pi -e 's/\$BASE_DIR\$/$ENV{"BASE"}/g' $NEW_FILE
	echo "Created $NEW_FILE"
done
