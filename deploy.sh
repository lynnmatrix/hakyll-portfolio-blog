echo "===== Trying to Start SSH Agent ====="; eval "$(ssh-agent -s)" && ssh-add ~/.ssh/id_rsa && echo "===== SSH Agent Started; Building. ====="; stack build && stack exec site clean && git submodule init && git submodule update && cd _site/ && git checkout master && cd .. && stack exec site build && echo "===== Switching to _site/ =====" && cd _site/ && git status && git add --all && git commit -m "Update (`date '+%F %T %Z'`)" && git push origin master && echo "===== Switching to parent =====" && cd .. && git status && git add _site/ && git status && git commit -m "DEPLOY: Updated _site/ on (`date '+%F %T %Z'`)" && git push
