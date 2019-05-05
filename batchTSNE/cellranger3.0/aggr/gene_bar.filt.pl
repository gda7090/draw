my $ori_bar = shift;
open F, $ori_bar;
while(<F>){
    chomp;
    my @arr = split/,/,$_;
    my @filt = @arr[0..4504];
    print join(",",@filt)."\n";
}
close F;

