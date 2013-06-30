./site build
cd _site
git add .
git commit -m "deploy"
git push origin master
cd ..
git add .
git commit -m "deployed site"
