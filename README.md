# Markdown Preview
1. Install pip

``` shell
curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
python get-pip.py
```
2. Install grip and ispell

``` powershell
pip install grip --ignore-installed
brew install ispell
```
# Latex Setting
1. install latex
2. pip install Pygments

# Golang
1. install language server
``` shell
go get -u github.com/sourcegraph/go-langservern
```
# Java
1. install language server
``` shell
git clone https://github.com/eclipse/eclipse.jdt.ls.git
./mvnw clean verify
```
2. mvn gen eclipse project
``` shell
mvn eclipse:eclipse -DdownloadSources=true -DdownloadJavadocs=true
```
3. fix mac too many open file
https://content.nanobox.io/fixing-too-many-open-files-in-macos/

# GUN global
1. install global and the_silver_searcher
``` shell
brew install global
brew install the_silver_searcher
```

