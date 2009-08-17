$(PASESFILE).asc: $(PASESFILE)
	gpg --armor --sign --detach-sig -u $(LP_USER) $(PASESFILE)

lp-upload-package: $(PASESFILE).asc lp-login
	curl -X POST https://launchpad.net/pases/trunk/0.1/+adddownloadfile -b /tmp/my_cookie \
-F "field.description=$(DESCRIPTION)" \
-F "field.filecontent.used=" \
-F "field.filecontent=@$(PASESFILE);type=application/octet-stream" \
-F "field.signature.used=" \
-F "field.signature=@$(PASESFILE).asc;type=application/pgp-encrypted" \
-F "field.contenttype=INSTALLER" \
-F "field.contenttype-empty-marker=1" \
-F "field.actions.add=Upload"
	rm /tmp/my_cookie

lp-login:
	curl -v -X POST -c /tmp/my_cookie https://launchpad.net/pases/+login \
-F "loginpage_email=$(LP_USER)" \
-F "loginpage_password=$(LP_PASSWORD)" \
-F "loginpage_submit_login=Log In"
