#!/usr/bin/perl -w

$SUCCESS = 0;
$ERROR   = 1;

$exit_code = $SUCCESS;

# WHAT; Gather arguments
# WHY;  Connot proceed without them
#
if ( $exit_code == $SUCCESS  ) {

    while ( $ARGV[0]  ) {
        chomp( $ARGV[0]  );
        $key = $ARGV[0];
        $value = $ARGV[1];

        if (( $key eq "\-\-input_file" || $key eq "\-if"  )) {
            $input_file = $value;
        } elsif (( $key eq "\-\-output_file"  ) || ( $key eq "\-of"  )) {
            $output_file = $value;
        } elsif (( $key eq "\-\-regex_file"  ) || ( $key eq "\-rf"  )) { 
            $regex_file = $value;
        } elsif (( $key eq "\-\-mode"  ) || ( $key eq "\-m"  )) { 
            $mode = $value;
        } elsif (( $key eq "\-\-data_type"  ) || ( $key eq "\-dt"  )) { 
            $data_type = $value;
        } else { 
            $err_msg = "Invalid argument \"$ARGV[0]\"";
            $exit_code++;
            last;
        }

        shift @ARGV;
        shift @ARGV;
    }

}

if ( $exit_code == $SUCCESS ) {

    if (( $input_file eq "" ) || ( $output_file eq "" ) || ( $regex_file eq "" ) || ( $data_type eq "" )) {
        $err_msg = "Not enough input argument provided";
        $exit_code = $ERROR;
    }

}

# WHAT: Slurp in regexes
if ( $exit_code == $SUCCESS ) {

    if ( -e "$regex_file" ) {
        open( REGEX, "<$regex_file" );
        chomp( @regex = <REGEX> );
        close( REGEX );
    } else {
        $err_msg = "Could not locate regex file \"$regex_file\"\n";
        $exit_code = $ERROR;
    }

}

if ( $exit_code == $SUCCESS ) {
    open( OUTPUT, ">$output_file" );
    open( INPUT, "<$input_file" );

    while ( <INPUT> ) {
        chomp( $input_line = $_ );
        $is_comment = 0;
        $comment_prefix = "#";

        if (( $data_type eq "batch"  ) || ( $data_type eq "copy"  ) || ( $data_type eq "cics" )) {
            $comment_prefix = "      *";
            $this_line_length = length( $input_line );

            if ( $this_line_length > 6 ) {
                $seventh_character = substr( $input_line, 6, 1 );

                if ( $seventh_character eq "*" ) {
                    $is_comment = 1;
                }

            }
        
        } elsif (( $data_type eq "jcl" ) || ( $data_type eq "proc" )) {

            if ( $mode eq "pre" ) {
                $comment_prefix = "//*";

                if ( $input_line =~ /^\/\/\*/ ) {
                    $is_comment = 1;
                }

            }

            if ( $mode eq "post" ) {

                if ( $input_line =~ /^#/ ) {
                    $is_comment = 1;
                }

            }

        } else {

            if ( $input_line =~ /^#/ ) {
                $is_comment = 1;
            }

        }

        # Set default output_line
        $output_line = $input_line;

        # Process only if we are not a commented line
        if ( $is_comment == 0 ) {
            chomp( $right_now = `date +%Y%m%d` );

            # Check the input_line for each regex
            foreach $regex ( @regex ) {
                $regex =~ s/[\n\r\t]//g;

                # Valid regex lines start with '("'
                if ( $regex =~ /^\(\"/ ) {
                    ( $src_regex , $dst_regex ) = split( / . /, $regex );
                    chomp( $src_regex );
                    chomp( $dst_regex );

                    # Flush parentheses and quotes
                    $src_regex =~ s/^\(//g;
                    $src_regex =~ s/\"//g;
                    $src_regex =~ s/\(/\\\(/g;
                    $src_regex =~ s/\)/\\\)/g;
                    #$src_regex =~ s/\)//g;
                    #$dst_regex =~ s/\(//g;
                    $dst_regex =~ s/\"//g;
                    $dst_regex =~ s/\)$//g;
                    $dst_regex =~ s/\(/\\\(/g;
                    $dst_regex =~ s/\)/\\\)/g;

                } else {
                    $src_regex = "";
                    $dst_regex = "";
                }

                # Don't do anything if src_regex is blank
                if ( $src_regex ne "" ) {

                    # Only do something if line matches src_regex
                    if ( $input_line =~ /$src_regex/ ) {

                        # If dst_regex is blank then convert this line to a comment
                        if ( $dst_regex eq "" ) {
                            # Max line length is 72 characters
                            $output_line = "$comment_prefix $right_now - Line commented out by script:";
                            $output_line = $output_line . "\n$comment_prefix $0";

                            # strip any leading spaces
                            $input_line =~ s/^\s+//g;
                            $new_line = $comment_prefix . " " . $input_line;
                            $line_length = length( $new_line );

                            # Build an array of lines if the total line length exceeds 72 characters
                            if ( $line_length > 71 ) {
                                $counter = 0;
                                @new_line_array = "";

                                while ( $line_length > 71 ) {
                                    $new_line_array[ $counter ] = substr( $new_line, 0, 71 );
                                    $line_remainder = substr( $new_line, 71 );
                                    $new_line = $comment_prefix . " " . $line_remainder;
                                    $line_length = length( $new_line );
                                    $counter++
                                }

                                if ( $new_line ne "" ) {
                                    $new_line_array[ $counter ] = $new_line;
                                }

                                foreach $new_line_array_element ( @new_line_array ) {
                                    $output_line = $output_line . "\n$new_line_array_element";
                                }

                            } else {
                                $output_line = $output_line . "\n$new_line";
                            }

                        } else {
                            $original_line = $input_line;

                            # KSH - make it a comment
                            if ( $comment_prefix eq "#" ) {
                                $original_line = "#" . $input_line;
                            }

                            # Cobol - make it a comment
                            if ( $comment_prefix eq "      *" ) {

                                # Flush character positions 72-79 from $input_line
                                $input_line =~ s/(?<=.{72})(.)/\ $1/s;
                                $input_line =~ s/(?<=.{73})(.)/\ $1/s;
                                $input_line =~ s/(?<=.{74})(.)/\ $1/s;
                                $input_line =~ s/(?<=.{75})(.)/\ $1/s;
                                $input_line =~ s/(?<=.{76})(.)/\ $1/s;
                                $input_line =~ s/(?<=.{77})(.)/\ $1/s;
                                $input_line =~ s/(?<=.{78})(.)/\ $1/s;
                                $input_line =~ s/(?<=.{79})(.)/\ $1/s;
                                
                                # Flush character positions 0-5 from $original_line
                                # and set position 6 as the comment character
                                $original_line =~ s/(?<=.{0})(.)/\ /s;
                                $original_line =~ s/(?<=.{1})(.)/\ /s;
                                $original_line =~ s/(?<=.{2})(.)/\ /s;
                                $original_line =~ s/(?<=.{3})(.)/\ /s;
                                $original_line =~ s/(?<=.{4})(.)/\ /s;
                                $original_line =~ s/(?<=.{5})(.)/\ /s;
                                $original_line =~ s/(?<=.{6})(.)/\*/s;
                            }

                            # JCL - make it a comment
                            if ( $comment_prefix eq "//*" ) {
                                $original_line =~ s/(?<=.{0})(.)/\//s;
                                $original_line =~ s/(?<=.{1})(.)/\//s;
                                $original_line =~ s/(?<=.{2})(.)/\*$1/s;
                            }

                            $input_line =~ s/$src_regex/$dst_regex/g;
                            # Fix parentheses
                            $input_line =~ s/\\\(/\(/g;
                            $input_line =~ s/\\\)/\)/g;
                            
                            $output_line = "$comment_prefix $right_now - The following line was modified";
                            $output_line = $output_line . "\n$comment_prefix by automation script:";
                            $output_line = $output_line . "\n$comment_prefix $0";
                            $output_line = $output_line . "\n$comment_prefix Original line was:";
                            $output_line = $output_line . "\n$original_line";
                            $output_line = $output_line . "\n$input_line";
                        }

                    }

                }

            }

        }

        print OUTPUT "$output_line\n";
    }

    close( INPUT );
    close( OUTPUT );
}

if ( $exit_code ne $SUCCESS ) {

    if ( $err_msg ne "" ) {
        print "    ERROR:  $err_msg ... processing halted\n";
    }

}

exit $exit_code;
