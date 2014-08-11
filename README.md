rAWS
====

Currently, this is a OS X and linux based package only; It isn't tested for windows. For the functions in this package to work correctly:

* Ensure that AWS has been correctly added to the enviroment variable `PATH`. See `add.aws.path` helper function provided part of this package.
* Ensure that you set up the config file correctly, which is contained in your `.aws` folder (by default the `.aws` folder should be set up at `~/.aws`).  Specifically, in your config file, you must specify the *aws_access_key_id*; the *aws_secret_access_key*; and optionally the default region - unless you specify these value by setting the enviroment variables. For more info, see AWS CLI github [readmepage](https://github.com/aws/aws-cli#other-configurable-variables)

For installation of AWS CLI on windows, the reader should read the following blog post http://keithxm23.blogspot.ca/2013/10/how-to-set-up-aws-command-line.html