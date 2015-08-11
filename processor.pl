#!/usr/bin/perl

$SUCCESS = 0;
$ERROR   = 1;

$exit_code = $SUCCESS;

# WHAT; Gather arguments
# WHY;  Connot proceed without them
#
if ( $exit_code == $SUCCESS  )

    while ( $ARGV[0]  ) {
        chomp( $ARGV[0]  );
        $key = $ARGV[0];
        $value = $ARGV[1];

        if (( $key eq "\-\-input_file" || $key eq "\-if"  )) {
            $input_file = $value;
        } elsif (( $key eq "\-\-output_file"  ) || ( $key eq "\-of"  )) {
            $output_file = $value;
        } elsif (( $key eq "\-\-src_regex"  ) || ( $key eq "\-sx"  )) { 
            $src_regex = $value;
        } elsif (( $key eq "\-\-dst_regex"  ) || ( $key eq "\-dx"  )) { 
            $dst_regex = $value;
        } elsif (( $key eq "\-\-type"  ) || ( $key eq "\-t"  )) { 
            $dst_regex = $value;
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

    if (( $input_file eq "" ) || ( $output_file eq "" ) || ( $src_regex eq "") || ( $dst_regex eq "" ) || ( $type eq "" )) {
        $err_msg = "Not enough input argument provided";
        $exit_code = $ERROR;
    }

}

if ( $exit_code == $SUCCESS ) {
    # Call perl script here with --input_file "${source_code_dir}/${target_file}", --output_file "${tmp_file}" --type "${target}" --src_regex "${left_side}" --dst_regex "${right_side}"
    open( OUTPUT, ">$output_file" );
    open( INPUT, "<$input_file" );

    while ( <INPUT> ) {
        chomp( $input_line = $_ );
        $is_comment = 0;
        $comment_prefix = "#";

        if (( $type eq "batch"  ) || ( $type eq "copy"  ) || ( $type eq "cics" )) {
            $seventh_character = substr( $input_line,6,1 );

            if ( $seventh_character eq "*" ) {
                $is_comment = 1;
                $comment_prefix = "      *";
            }
        
        } elsif (( $type eq "jcl" ) || ( $type eq "procs" )) {

            if ( $input_line =~ /^\/\/\*/ ) {
                $is_comment = 1;
                $comment_prefix = "//*";
            }

        } else {

            if ( $input_line -~ /^#/ ) {
                $is_comment = 1;
            }

        }

        if ( $is_comment == 0 ) {
            print OUTPUT_FILE "$input_line\n";
        } else {
            $right_now = `date +%Y%m%d`;

            if ( $src_regex eq "" ) {
                print OUTPUT_FILE "$input_line\n";
            } else {

                if ( $dst_regex eq "" ) {
                    # Max line length is 72 characters
                    print OUTPUT_FILE "$comment_prefix $right_now - Line commented out by $0\n";
                    # strip any leading spaces
                    $new_line =~ s/^\s+//g;
                    $new_line = $comment_prefix . " " . $input_line;
                    $counter = 0;
                    $line_length = length( $new_line );

                    while ( $line_length > 0 ) {
                        $new_line_array[ $counter ] = substr( $new_line, 0, 71 );
                        $new_line = substr( $new_line, 72 );
                        $new_line = $comment_prefix . " " . $new_line;
                        $line_length = length( $new_line );
                        $counter++
                    }

                    foreach $output_line ( @new_line_array ) {
                        print OUTPUT_FILE "$output_line\n";
                    }

                } else {
                    $original_line = $input_line;
                    # strip any leading spaces
                    $original_line =~ s/^\s+//g;
                    $input_line =~ s/$src_regex/$dst_regex/g;
                    print OUTPUT_FILE "$comment_prefix $right_now - The following line was commented out\n";
                    print OUTPUT_FILE "$comment_prefix by automation script $0\n";
                    print OUTPUT_FILE "$comment_prefix Original line was:\n";
                    print OUTPUT_FILE "$comment_prefix $original_line\n";
                    print OUTPUT_FILE "$input_line\n";
                }
            
            }

        }

    }

}

exit $exit_code;
