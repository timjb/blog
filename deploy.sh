./site build
cd deploy
git checkout master
git pull origin master
find . -maxdepth 1 -not -name '.git' | xargs rm -rf
cp -rf ../_site/* .
git add -A .
git commit -m "deploy"
git push origin master
cd ..
git add -A .
git commit -m "deployed site"
