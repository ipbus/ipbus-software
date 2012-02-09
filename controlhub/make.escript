#!/usr/bin/env escript


main(_) ->
  % Make the ebin directory if it doesn't already exist
  os:cmd("mkdir -p ebin"),
  % Makes everything as per the options found in the Emakefile
  case make:all() of
    up_to_date ->
      io:format("Build is up-to-date.~n"),
      halt(0);  % If compilation works, return success code to host OS
    error ->
      io:format("Error during build!~n"),
      halt(1)        % If compialtion fails, return error code to host OS
  end.
