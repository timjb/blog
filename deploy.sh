./site build
cd deploy
find . -maxdepth 1 -not -name '.git' | xargs rm -rf
cp -rf ../_site/* .
git add .
git commit -m "deploy"
git push origin master
cd ..
git add .
git commit -m "deployed site"
