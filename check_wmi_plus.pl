#!/usr/bin/perl -w
#
# check_wmi_plus.pl - nagios plugin for agentless checking of Windows
#
# ##################
#  UPDATED by steav
#
# new MODE = checkfileage (Unit: hours)
# new MODE = checkdrivespace (Units: %, GB, MB...)
# checkprocess: Warning/Critical-Message + comma-separated list of processes possible
# checkprocess: No more LIKE (now = ) - sometimes causes error 0x80041017
#
# ##################
#
# Copyright (C) 2011 Matthew Jurgens
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#

my $VERSION="1.31";

use strict;
use Getopt::Long;
use vars qw($PROGNAME);
use lib "/usr/lib64/nagios/plugins";
use lib "/root/perl5/lib/perl5/x86_64-linux-thread-multi/auto"; # CHANGE THIS IF NEEDED
use utils qw ($TIMEOUT %ERRORS &print_revision &support);
# use Data::Dumper;
  
my $opt_Version='';
my $opt_host='';
my $opt_help='';
my $opt_mode='';
my $opt_username='';
my $opt_password='';
my $opt_arguments='';
my $opt_other_arguments='';
my $opt_warn='';
my $opt_critical='';
my $opt_timeout='';
my $opt_bytefactor=1024;
my $debug=0;
my $opt_new='';
my $opt_value='';
my $opt_z='';

my ($wmi_commandline, $output, $output_crit, $output_warn);
$PROGNAME="check_wmi_plus";

my $wmic_command="/bin/wmic"; # CHANGE THIS IF NEEDED
if (! -x $wmic_command) {
   print "This plugin requires the linux implementation of wmic eg from zenoss.\nOnce wmic is installed, configure its location by setting the \$wmic_command variable in this plugin.";
   exit $ERRORS{"UNKNOWN"};
}

# list all modes here - this makes sure only valid modes are called
# all the modes that require a critical/warning specification set to value of 1
my %mode_list = ( 
   checkservice   => 1,
);

# multipliers are calculated as BYTEFACTOR^mulitpler eg m = x * 1000^2 or x * 1024^2
my %multipliers=(
   k  => 1,
   m  => 2,
   g  => 3,
   t  => 4,
   p  => 5,
   e  => 6,
);

Getopt::Long::Configure('no_ignore_case');
GetOptions(
   "Version"      => \$opt_Version,
   "help"         => \$opt_help,
   "mode=s"       => \$opt_mode,
   "Hostname=s"   => \$opt_host,
   "username=s"   => \$opt_username,
   "password=s"   => \$opt_password,
   "arguments=s"  => \$opt_arguments,
   "otherguments=s"=>\$opt_other_arguments,
   "warning=s"    => \$opt_warn,
   "critical=s"   => \$opt_critical,
   "timeout=i"    => \$opt_timeout,
   "bytefactor=s" => \$opt_bytefactor,
   "debug"        => \$debug,
   "value=s"      => \$opt_value,
   "z"            => \$opt_z,
   );
 
if ($opt_timeout) {
   $TIMEOUT=$opt_timeout;
}
 
if ($opt_bytefactor ne '1024' && $opt_bytefactor ne '1000') {
   print "The BYTEFACTOR option must be 1024 or 1000. '$opt_bytefactor' is not valid.\n";
   short_usage();
}

# Setup the trap for a timeout
$SIG{'ALRM'} = sub {
   print "UNKNOWN - Plugin Timed out ($TIMEOUT sec)\n";
   exit $ERRORS{"UNKNOWN"};
};
alarm($TIMEOUT);
 
if ($opt_help) {
   usage();
}
if ($opt_Version) {
   print "Version: $VERSION\n";
   exit $ERRORS{'OK'};
}

if ($opt_warn && $opt_critical && $opt_value) {
   my ($test_result,$neww,$newc)=test_limits($opt_warn,$opt_critical,$opt_value);
   print "Overall Status Generated = $test_result ($neww,$newc)\n";
   exit $test_result;
}

if (! $opt_host) {
   print "No Hostname specified\n\n";
   short_usage();
}
 
#if ($mode_list{$opt_mode} && !$opt_critical) {
#   print "No critical threshold specified\n\n";
#   short_usage();
#}
#
#if ($mode_list{$opt_mode} && !$opt_warn) {
#   print "No warning threshold specified\n\n";
#   short_usage();
#} 

# ahora ejecuta el sub apropiado para el cheque
if (defined($mode_list{$opt_mode})) {
# tenemos que establecer una referencia a la subrutina ya que se establece una referencia estricta
   my $subref=\&$opt_mode;
   &$subref('');
} else {
   print "Se debe especificar un MODO válido\n";
   short_usage();
}

# si llegamos hasta aquí, por defecto saldremos OK
exit $ERRORS{'OK'};

#-------------------------------------------------------------------------
sub short_usage {
my ($no_exit)=@_;
print <<EOT;
Uso: -H HOSTNAME -u DOMAIN/USER -p PASSWORD -m MODE [-b BYTEFACTOR] [-a ARG ] [-w WARN] [-c CRIT] [-o OTHERARG] [-t TIMEOUT] [-d] [-z]
EOT
if (!$no_exit) {
   print "Specify the --help parameter to view the complete help information\n";
   exit $ERRORS{'UNKNOWN'};
}
}
#-------------------------------------------------------------------------
sub usage {
short_usage(1);
print <<EOT;

dónde
BYTEFACTOR es 1000 o 1024 y se utiliza para unidades de conversión, por ejemplo, bytes a GB. El valor predeterminado es 1024.
EL TIEMPO DE ESPERA está en segundos
-d Habilitar depuración
-z Proporciona advertencias de especificación completa y valores críticos para los datos de rendimiento.
   No todos los programas de procesamiento de datos de rendimiento pueden manejar esto, por ejemplo, PNP4Nagios.


MODE=checkservice
-----------------
   ARG: el nombre corto o largo del servicio que se puede ver en las propiedades del servicio en Windows
      Se pueden utilizar expresiones regulares. Utilice Auto para comprobar que todos los servicios iniciados automáticamente estén bien.
   WARN/CRIT se puede utilizar como se describe a continuación. Coinciden con el número de procesos.
   OTHERARG se puede especificar como "bueno", "malo" (el valor predeterminado) o total y hace que los valores WARN/CRIT coincidan con el número
      de servicios que están en un estado "bueno" (funcionando bien), en un estado "malo" (detenido/fallado, etc.), o el número total de
      servicios coincidentes. eg -a Exchange -o bad -c 0


WARNING and CRITICAL Specification:
===================================

If warning or critical specifications are not provided then no checking is done and the check simply returns the value and any related performance data. If they are specified then they should be formatted as shown below.

A range is defined as a start and end point (inclusive) on a numeric scale (possibly negative or positive infinity). The theory is that the plugin will do some sort of check which returns back a numerical value, or metric, which is then compared to the warning and critical thresholds. 

This is the generalised format for ranges:
[@]start:end

Notes:
   1. start = end
   2. start and ":" is not required if start=0
   3. if range is of format "start:" and end is not specified, assume end is infinity
   4. to specify negative infinity, use "~"
   5. alert is raised if metric is outside start and end range (inclusive of endpoints)
   6. if range starts with "@", then alert if inside this range (inclusive of endpoints)

Example ranges

warn/CRIT definition    Generate an alert if x...
10                      < 0 or > 10, (outside the range of {0 .. 10})
10:                     < 10, (outside {10 .. 8})
~:10                    > 10, (outside the range of {-8 .. 10})
10:20                   < 10 or > 20, (outside the range of {10 .. 20})
\@10:20                 = 10 and = 20, (inside the range of {10 .. 20})
\@10                    >= 0 and <= 10, (inside the range of {0 .. 10})

EOT

my $modelist='';
for my $mode (sort keys %mode_list) {
   if ($mode_list{$mode}) {
      $modelist.="$mode, "
   }
}
$modelist=~s/, $/./;
if ($modelist) {
   print "\nWARN and/or CRIT can be used for the following MODES: $modelist\n";
}

exit $ERRORS{'UNKNOWN'};
}
#-------------------------------------------------------------------------
sub display_uptime {
# pass in an uptime string
# if it looks like it is in seconds then we convert it to look like days, hours minutes etc
my ($uptime_string)=@_;
my $new_uptime_string=$uptime_string;
if ($uptime_string=~/^[0-9\.]+$/) {
   # its in seconds, so convert it
   my $uptime_minutes=sprintf("%d",$uptime_string/60);
   my $uptime=$uptime_string;
   my $days=int($uptime/86400);
   $uptime=$uptime%86400;
   my $hours=int($uptime/3600);
   $uptime=$uptime%3600;
   my $mins=int($uptime/60);
   $uptime=$uptime%60;

   my $day_info='';
   if ($days==1) {
      $day_info="$days day";
   } elsif ($days>1) {
      $day_info="$days days";
   }
   $new_uptime_string="$day_info " . sprintf("%02d:%02d:%02d (%smin)",$hours,$mins,$uptime,$uptime_minutes);
}
return $new_uptime_string; 
}
#-------------------------------------------------------------------------
sub scaled_bytes {
   # from http://www.perlmonks.org/?node_id=378538
   # very cool
(sort { length $a <=> length $b }
map { sprintf '%.3g%s', $_[0]/$opt_bytefactor**$_->[1], $_->[0] }
[" bytes"=>0],[KB=>1],[MB=>2],[GB=>3],[TB=>4],[PB=>5],[EB=>6])[0]
}
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
sub checkservice {
# ------------------------ comprobando todos los servicios
my $where_bit='';
my $auto_mode='';
if (lc($opt_arguments) eq 'auto') {
   # for this query we need to look for all automatic services
   # check that all auto services are 
   # STARTED=True, STATE=Running and STATUS=OK
   # we do a query that actually always should return data so that we know that the query works
   # we could do a select just listing the bad ones, but it returns nothing if good. hard to tell if it really worked ok.
   $where_bit="where StartMode='auto'";
   $auto_mode=1;
} else {
   # for this query we have been passed a regex and must look for that
   # so the WMI query should return all services and then we will apply the regex
   # this is the default
}

# wmic returns something like:
# CLASS: Win32_Service
# DisplayName|Name|Started|StartMode|State|Status
# Telnet|TlntSvr|False|Auto|Stopped|OK
# Security Center|wscsvc|True|Auto|Running|OK

$wmi_commandline = "$wmic_command -U ${opt_username}%${opt_password} //$opt_host \"Select displayname, Started, StartMode, State, Status FROM Win32_Service $where_bit\"";
$output = `$wmi_commandline 2>&1`;
$debug && print "QUERY: $wmi_commandline\nOUTPUT: $output\n";

# position the regular expression match at the start of where the first line of services should be
if ($output=~/State\|Status\n/sg) {
   my $result_text='';
   # print "$output\n";
   # now loop through the results, showing the ones requested
   my $num_ok=0;
   my $num_bad=0;
   while ($output=~/(.*?)\|(.*?)\|(.*?)\|(.*?)\|(.*?)\|(.*?)\n/sg) {
      my $displayname=$1;
      my $name=$2;
      my $started=$3;
      my $startmode=$4;
      my $state=$5;
      my $status=$6;
      my $state_spanish = '';

      #traducciones de respuestas en los estados del servicio a chequear

      if ($state eq 'Running') {
         $state_spanish = 'Ejecutando';
         } 
         elsif ($state eq 'Warning') {
            $state_spanish = 'Advertencia';
         } 
         elsif ($state eq 'Stopped') {
            $state_spanish = 'Detenido';
         } 
         elsif ($state eq 'Unknown') {
            $state_spanish = 'Desconocido';
         } 
         else {
            $state_spanish = $state;
      }

      # print "FOUND:$1,$2,$3,$4,$5,$6\n";
      if (  $auto_mode || 
            ( !$auto_mode && ($displayname=~/$opt_arguments/i || $name=~/$opt_arguments/i) ) 
         ) {
         if ($started eq 'True' && $state eq 'Running' && $status eq 'OK') {
            $num_ok++;
            if (!$auto_mode) {
               # if we have using the regex mode then list out the services we find
               $result_text.="$displayname ($name) esta $state_spanish, ";
            }
         } else {
            $num_bad++;
            $result_text.="$displayname ($name) esta $state_spanish, ";
         }
      }
   }
   
   $result_text=~s/, $/./;

   my $num_total=$num_ok+$num_bad;
   my $check_value=$num_bad;
   my $check_description='';
   if ($opt_other_arguments eq 'good') {
      $check_value=$num_ok;
   } elsif ($opt_other_arguments eq 'total') {
      $check_value=$num_total;
   }

   my ($test_result,$warn_perf_spec,$critical_perf_spec)=test_limits($opt_warn,$opt_critical,$check_value);

   # performance data should always be in a fixed unit for consistency
   my $performance_data="'Recuento total de servicios'=${num_total}; 'Estado correcto del recuento de servicios'=${num_ok}; 'Estado del problema del recuento de servicios'=${num_bad}; ";

   print "Encontrado $num_total servicio(s). $num_ok OK y $num_bad con problemas. $result_text|$performance_data\n";
   $debug && print "Checking Warn/Crit against $opt_other_arguments. Check $check_value against warn:$opt_warn and crit:$opt_critical\n";

   exit $test_result;

} else {
   # could not find what we were looking for - some kind of error
   print "UNKNOWN: " . $output;
   exit $ERRORS{'UNKNOWN'};
}

}
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------

#-------------------------------------------------------------------------

#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
sub apply_multiplier {
# multiply a value up using a mulitplier string value
# pass in
# a value
# a multiplier eg k, m, g etc - might be empty
my ($value,$multiplier)=@_;
if ($multiplier) {
   $debug && print "Value of $value ";
   $value=$value * $opt_bytefactor ** $multipliers{lc($multiplier)};
   $debug && print "multiplied up to $value using $multiplier ($opt_bytefactor ^ " . $multipliers{lc($multiplier)} . ")\n";
}
return $value;
}
#-------------------------------------------------------------------------
sub test_single_boundary {
# test a value against a single boundary. The boundary should have already been parsed
# pass in
# less_than_boundary - set to < if test should be less than boundary
# equal - set to = if test should include an = boundary
# boundary value
# boundary multiplier character eg k, m, g etc
# the test value
# 
# return 1 if boundary exceeded or zero if not
# also return the actual mulitplied up $boundary_value
my ($less_than_boundary,$boundary_equal,$original_boundary_value,$boundary_multiplier,$test_value)=@_;

my $test_result=0;

my $boundary_value=apply_multiplier($original_boundary_value,$boundary_multiplier);

if ($less_than_boundary && $boundary_equal) {
   # TEST <=
   $debug && print "TEST1 $test_value <= $boundary_value\n";
   if ($test_value <= $boundary_value) {
      $test_result=1;
   }
} elsif ($less_than_boundary) {
   # TEST <
   $debug && print "TEST2 $test_value < $boundary_value\n";
   if ($test_value < $boundary_value) {
      $test_result=1;
   }
} elsif ($boundary_equal) {
   # TEST >=
   $debug && print "TEST3 $test_value >= $boundary_value\n";
   if ($test_value >= $boundary_value) {
      $test_result=1;
   }
} else {
   # TEST > 
   $debug && print "TEST4 $test_value > $boundary_value\n";
   if ($test_value > $boundary_value) {
      $test_result=1;
   }
}

$debug && print "Test of $less_than_boundary$boundary_equal$original_boundary_value$boundary_multiplier ($boundary_value) vs $test_value yields $test_result\n";
return $test_result,$boundary_value;
}



#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
sub parse_limits {
my ($spec,$test_value)=@_;
# we return zero if the value does not trigger the spec
$debug && print "Testing $test_value against SPEC: $spec\n";
my $test_result=0;

# we need a warning/critical value for performance data graphs
# for single values it is easy, its just the boundary value specified
# for ranges we use the max of the range - maybe this is not always right
my $perf_data_spec='';

if ($spec ne '') {
   my $at_specified='';
   my $min='';
   my $min_multiplier='';
   my $max='';
   my $max_multiplier='';

   my $format_type=0;

   if ($spec=~/(\@*)([0-9+\-\.\~]*)([KMGTPE]*):([0-9+\-\.\~]*)([KMGTPE]*)/i) {
      $at_specified=$1;
      $min=$2;
      $min_multiplier=$3;
      $max=$4;
      $max_multiplier=$5;
      $format_type=1;
   } elsif ($spec=~/(\@*)([0-9+\-\.\~]+)([KMGTPE]*)/i) {
      $at_specified=$1;
      $min=0;
      $min_multiplier='';
      $max=$2;
      $max_multiplier=$3;
      $format_type=2;
   }

   if ($format_type) {
      $debug && print "Range Spec=$at_specified,$min,$min_multiplier,:,$max,$max_multiplier\n";
      # there should always be a max value and may not be a min value
      my $lower_bound_value='';
      my $upper_bound_value='';
      my $lower_bound_check='';
      my $upper_bound_check='';

      # there is a possibility that the boundary is specified as ~
      # this means negative infinity

      # we have a range comparison and we check both bounds using < and >
      if ($min eq '~') {
         # since min is negative infinity then no point in doing this lower bound test as it will be always false
         $lower_bound_check=0;
         $lower_bound_value='~';
      } else {
         ($lower_bound_check,$lower_bound_value)=test_single_boundary('<','',$min,$min_multiplier,$test_value);
      }
      
      if ($max eq '') {
         # since max is inifinity no point in checking since result will always be false
         $upper_bound_check=0;
         $upper_bound_value='';
      } else {
         ($upper_bound_check,$upper_bound_value)=test_single_boundary('','',$max,$max_multiplier,$test_value);
      }
      # generate alert if either of these are triggered
      if ($lower_bound_check || $upper_bound_check) {
         $test_result=1;
      }

      if ($at_specified) {
         # this just reverses the results
         if ($test_result==1) {
            $test_result=0;
         } else {
            $test_result=1;
         }
         $debug && print "@ specified so reverse the result\n";
      }

      # rewrite the specification taking into account any multipliers
      if ($format_type==1) {
         if ($opt_z) {
            #  provide full spec performance warn/crit data
            $perf_data_spec="$at_specified$lower_bound_value:$upper_bound_value";
         } else {
            # provide partial spec performance warn/crit data
            # if only one number has been specified in the range spec then use that
            # otherwise use the upper bound value
            $perf_data_spec="$upper_bound_value";
            if ($upper_bound_value=~/[0-9+\-\.]+/ && $lower_bound_value=~/[0-9+\-\.]+/) {
               # stick with only upper bound data
            } elsif ($lower_bound_value=~/[0-9+\-\.]+/) {
               # no upper bound specified so use the lower bound
               $perf_data_spec="$lower_bound_value";
            }
         }
      } else {
         # for this format type the min was forced to zero, but it was not actually specified - so we only show an upper bound 
         if ($opt_z) {
            #  provide full spec performance warn/crit data
            $perf_data_spec="$at_specified$upper_bound_value";
         } else {
            # provide partial spec performance warn/crit data
            $perf_data_spec="$upper_bound_value";
         }
      }

   } else {
      # seems to be some invalid spec format
      $test_result=100;
   }
}

$debug && print "Test Result = $test_result\n";
return $test_result,$perf_data_spec;
}
#-------------------------------------------------------------------------
sub test_limits {
my ($warn_spec,$critical_spec,$test_value)=@_;
$debug && print "Testing $test_value against WARN: $warn_spec and CRIT: $critical_spec\n";

my $test_result=$ERRORS{'UNKNOWN'};

$debug && print "----- Critical Check -----\n";
my ($critical_result,$critical_perf)=parse_limits($critical_spec,$test_value);
$debug && print "----- Warning Check -----\n";
my ($warn_result,$warn_perf)=parse_limits($warn_spec,$test_value);
$debug && print "-------------------------\n";

if ($critical_result>1) {
   print "Critical specification not defined correctly\n";
} elsif ($warn_result>1) {
   print "Warning specification not defined correctly\n";
} elsif ($critical_result==1) {
   $test_result=$ERRORS{'CRITICAL'};
} elsif ($warn_result==1) {
   $test_result=$ERRORS{'WARNING'};
} else {
   $test_result=$ERRORS{'OK'};
}

return $test_result,$warn_perf,$critical_perf;
}
#-------------------------------------------------------------------------
sub max {
# passed in a list of numbers
# determaxe the maximum one 
my($max_so_far) = shift @_;  # the first one is the smallest yet seen
foreach (@_) {               # look at the remaining arguments
  if ($_ > $max_so_far) {    # could this one be smaller
    $max_so_far = $_;
  }
}
return $max_so_far;
}
#-------------------------------------------------------------------------

