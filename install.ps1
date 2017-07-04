$path = ".\elpa\"
If(Test-Path -path $path)
{
    Remove-Item -path $path -Recurse
}

emacs -Q --batch -l .\init.el -f clojure-mode
