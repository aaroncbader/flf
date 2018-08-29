Readme updated March 8, 2017

This is a field line follower for general geometry. Originally designed for
HSX it can handle any arbitrary 3d geometry. Input currently can be in the
form of coils where the integration is a biot-savart integration over the coil
segments, or from a precomputed magnetic grid (currently input in text form,
.nc support from mgrid files to be coming shortly.)

Input is in the form of a namelist, currently must be named 'flf.namelist' the
parameters will be included below. The main section of the namelist is
designated with flf flag, and there may be additional sections. A description
of the namelist parameters and necessary auxiliary files are below.

The output is in two forms. The output file gives limited information for each
point usually in the form of where it started, where it stopped, and whether
it hit anything. In addition most versions of the code print out to the screen
more detailed information, including position at each step. 

--------NAMELIST PARAMETERS-----------

general_option (default 1): This is the type of operation for the code. The
following options are currently available.

1 - field line following (default)
2 - no following is done, the magnetic field components Bx, By, Bz along with
the |B| are output for each input point
3 - calculation of epsilon effective. This is slow unless a magnetic grid is
specified.
0 - Tests - requires HSX coil, vessel and divertor files.

output_coils: (default 0) If set to 1, the code will write a coils.out file
with a representation of all the coils and the associated currents. Useful for
debugging to make sure that all the coils are being replicated properly and
the currents are going in the correct directions.

points_file: (default 'points.in') Name of the file to load the starting
points, see below for the description of the format for this file.

points_number: (default 1) Number of points to load

n_iter: (default 1) for options 1 (field line following) and 3 (epsilon
effective) this is the number of iterations to call. This is unused for
magnetic field output.

points_dphi: distance in phi to travel for each iteration.

follow_type: there are different options for following the field
follow_type=1: use phi as the independent variable
follow_type=2: use arclength as the independent variable
follow_type=3: use chi = dl dot B as the independent variable
follow_type=4: use gboozer (?) as the independent variable

in all cases points_dphi represents the follow distance in the appropriate variable. dphi is a legacy name and it will be changed at some point

field_type: This specifies the input type for the field, there are three options:
field_type='coils' is coil files (see below for format), 
field_type='ascii' is an ASCII magnetic grid file (see below for format)
field_type='netcdf' is a .nc file created by xgrid 

in the case of 'ascii' or 'netcdf' files num_main_coils should be set to 0

num_main_coils: (necessary) This is the number of coil files. If 1 or greater
a &coils section must be included with the names of the coil files in the
namelist. If 0, then mgrid_file must be set. See below for formatting
information for both of these.

mgrid_file: Name of the mgrid file, needed if num_main_coils is 0 or less.

num_periods: (default 4) Number of periods. Coil files or mgrid files only
need to be included for one period. The code will replicate for other periods.

is_mirrored: (default 0) For 4 period machines there is an option to only
include half a period worth of coils. (this does not work for different
periodicities.) Set this to 1 for mirroring.

skip_value: (default 1) For large coil files it is sometimes useful to not
read in every segment in order to speed up computation. Set this to N to only
read in every Nth point.

main_winding: (default 1) Use if the coil file includes multiple
windings. This is necessary for proper calculation of aux coils which tend to
have currents scaled to total values of the main current. See the description
for aux_percent in the coil section for an explanation of how this works.

vessel_file: (default '') Set to a vessel file name in order to activate
vessel termination. If a vessel file is included, the code will check at each
step to verify that the point is inside the vessel. If it is found outside,
the point will terminate and the termination location will be reported.

results_file: (default 'results.out') Place to store the point beginning and
termination output.

num_aux_coils: (default 0) Number of auxiliary coils to include. If this is
set to nonzero an aux_file must be included.

aux_file: file to read the aux coils. See below for format information

num_divertors: (default 0) Number of divertors to include. If set to greater
than 0, a &div section must be included in the namelist. See below.

mag_axis: (default '') Must be included if divertors are set (or Boozer
diffusion is set, see below). Gives the location of a magnetic axis file.

num_limiters: (default 0) Number of limiters, if set a &lim must be included.

use_diffusion (default 0): Set to 1 or 2 to include diffusion. 1 is random
perpendicular diffusion and 2 is boozer's spiraling out method. 

diffusion_species: (default 0): 0 for protons, 1 for electrons, used for
diffusion option 1.

d_perp: For diffusion option 1, this is the perpendicular diffusion parameter
in units of m^2/s

temperature: For diffusion option 1, this is the temperature of the particle
in eV

boozer_step: For diffusion option 2, this is how large to spiral out on each
spiral out attempt.

boozer_phi: For diffusion option 2, this is how many iteration steps to take
before spiraling outward.  

lcfs_file (default 1): A description of the last closed flux surface. If this
is set the code will calculate the distance from the LCFS for each point and
record that instead of the magnetic field in the logged output.

In addition to the main section of the namelist additional sections are
required if coils, divertors or limiters are to be included. These sections
are separated because the size of the various arrays need to be initialized
based on values in the main namelist. This provides a convenient separation.

&coils section (needed if num_main_coils >= 1)

main_files: A list of all the coil files to load. The length must match that
given in num_main_coils

main_current_repeat (default 0): Set to 1 if all main coils have the same
current, then only a single value is needed for the main_current entry.

main_current: A list of the currents (in amps) for each of the main coils, or
a single value if main_current_repeat is set

aux_percent: A list of the percentage of current for each of the aux coils,
scaled to the current in coil 1, multiplied by the value in main_winding. For
example, if coil one in main_current has 1kA and main_winding is set to 10,
then an aux_percent of 1.0 would give 10kA in the aux coils, and aux_percent
of 0.01 would give 100 Amps. The length of aux_percent must match the number
in num_aux_coils

&div section

div_files: This is the only entry, it is a list of files for the divertor
plates. The number of entries must match the number in num_div_files. This is
only read if num_div_files >= 1

&lim section

lim_files: Similar to div_files.

----------------DESCRIPTION OF FILE INPUTS------------

POINTS INPUT FILE

This is the file referenced in points_file in the namelist. There
are two forms, one for the generic field line following, and one for the
epsilon effective calculation.  For field line following points are provided
in r,z,phi (in meters) with phi in radians. Example of two lines

1.45 0.0 0.0
1.46 1.0 3.14

Two points are given the first at r = 1.45, z = 0.0 and phi = 0.0, the second
at r = 1.46, z = 1.0 and phi = pi.

For epsilon effective, the calculation only works for points at the outboard
side of the symmetry midplane. (The code assumes phi=0 is a symmetry plane,
some modifications are required if that's not true.) The input is two columns
per line, the first is the radial value and the second is a calculation of
dpsi/dr (dpsi/dz and dpsi/dphi are both zero).

COIL FILES

These are used if num_main_coils is greater than 1

Coil files all have the following form. The first line is the number of points
in the coil file. The rest of the lines are the coordinates for each
consecutive coil vertex in x,y,z (meters). Format similar to points input.

AUXILIARY COIL FILES

If num_aux_coils are greater than 1, then an aux file needs to be
specified. The format is similar to the coil files, except only one file is
given for all the aux coils. The first line in the file is the number of coils
altogether. This needs to be greater than the value of num_aux_coils to read in.

MAGNETIC GRID FILES

Reading a .nc mgrid is not yet supported. Instead an ascii representation is
given. The grid is a r,z,phi grid with uniform spacing in each direction.  The
format is as follows.  First the r coordinates are given, from smallest to
largest, one per line. Then the Z coordinates are given, from smallest to
largest, one per line. Then the phi coordinates are given from smallest to
largest one per line.

Then all the Br values are given, then all the Bz values, then all the bphi
values. For each the values are given as Br(phi0, z0, r0), Br(phi0, z0, r1)
... Br(phi0, z0, rn), Br(phi0, z1, r0), Br(phi0, z1, r1) ... Br(phi0, zn, rn),
Br(phi1, z0, r0) and so on.

VESSEL FILE
  
A vessel needs to span the exact same range as the coils. So if num periods is
four the vessel needs to span from 0 to pi/2. The first line of the vessel
file is the number of phi slices, then the number of poloidal slices. This is
followed by all the vessel points in x,y,z. Ordering first goes through all
points on one phi slice, then the next one. It is important that the first
point and the last point match (the vessel closes on itself) and that the
vessel values are as close as possible to the preceding one (since
slices of intermediate phis are formed by interpolation).

DIVERTOR FILE

This file input is a bit different because it uses the same input format as
EMC3 divertor files. First line is not used. Second line has 5 parameters,
only the first two are used. The first is the number of toroidal slices, the
second is the amount of points in each slide. This is then followed by each
toroidal slice in order. The toroidal slices look like toroidal position in
degrees, then R and Z value (in cm!). 

MAGNETIC AXIS FILE

Like the vessel, the magnetic axis must span the entire toroidal range. The
first line is the number of toroidal slices, this is followed by that number
of lines with R and Z values of the magnetic axis for each slice.

LCFS FILE

This is the same format as the vessel file.

LIMITER FILE

At some point I'll ask Laurie to write this part up.




