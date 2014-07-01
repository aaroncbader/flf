This is a field line follower for the HSX geometry, it is designed to
follow field lines and check for termination on structures.

The code requires user defined coil files as well as files to determine
structures.  All setup information is given in an input file called
"flf.input." Starting points are given in a file specified in flf.input.

The input file does change with new versions, this guide is for the state of
the input file on Jul 1, 2014.

In the input file, lines that begin with ! are comment lines.  These are used
to orient the user and are ignored by the code.  You cannot insert a comment
midline after data.

The input file includes in order:
<filename for the points>
<Number of points from input file to follow>
<Distance in toroidal angle to follow points in an "explicit" step (see output
section)>
<Number of explicit steps to take>
<File for condensed data output>

<Number of periods and symmetry information, 4 1 is HSX.  All other symmetry
types cannot use stellarator symmetry and must define all coils for a period>
<Number of main field coils (N_main)>
<filenames for main field coils (N_main lines)>
<Skip value for main coils, only use 1 out of skip points to save
computational time for large coil files>
<Currents for main field coils (N_main lines)>

<File for auxiliary coils> <Number of auxiliary coils to load (N_aux), along
with a multiplication factor> 
<Currents for auxiliary coils (N_aux) lines
given as a multiplicative factor.  The actual current is given as a percentage
of the current in the first main field coil multiplied by the multiplication
factor in the previous input line>

<Include diffusion (1 for yes, 0 for no)>
<Diffusion option, 1 = electron, not 1 = proton>
<D_perp and background Te (eV!)>

Note if diffusion is set to 0, the code will read these files but not use
them.  Do not comment them out.

<Number of vessels to use, N_vess>
<Vessel file names>

<Number of limiters to use, N_lim>
<limiter file names>

<Number of divertors to use, N_div>
<Divertor file names (N_div) lines>
<Magnetic axis file>

----------input files---------

With the exception of the Divertor input which follows convention from another
code, all inputs are in SI units unless otherwise noted.

main coils are given in x,y,z coordinates, one coil per file.  The first line
is the number of points in the file.

auxiliary coils are all given in the same file.  Each coil is preceded by the
number of points in that coil.  As with main coils, the points are in x,y,z
coordinates.  The code will read in the first N coils, with N being a user set
number.  If you ask for more coils than exist in the file, it will crash.

All auxiliary coils follow the same symmetry rules as the main coils (although
there is a branch version which allows for asymmetric coils)

points input files are given in r,z,phi coordinates.  If you ask to read in
more points than exist in the file, it will crash.

There are three types of structures to calculate strikes.  These are described
below.

Vessel files:

The first line is the list of toroidal points and then the list of poloidal
points in the file.  The data is given in xyz coordinates.  (this may be moved
to R-Z in the future).  Starting with the toroidal plane at phi = 0, the
points on the vessel in this plane are given.  They must be poloidally closed,
with the last point being the same as the first.  Then the next toroidal slice
is mapped out.  The code will assume that the slices are equidistant.

The code will calculate whether a given point is inside or outside the vessel
at each implicit step.

Limiter files:

The first point is the number of points on the limiter

The next line is a vector (in x,y,z) which is normal to the plane of the
limiter

The third line is a point (in x,y,z) to be used as the origin of the limiter
plane.

Following this are the points in (x, y) in the limiter plane that define the
limiter. 

At each implicit step the code sees whether it is close enough to the
limiter.  If it is close enough it projects it onto the plane and checks if it
is inside the mapped out structure.

Divertor files

The divertor file information is the same used for the EIRENE code (for my
sanity).  The input is somewhat different than other ones for this reason.
The input is as follows.

The first line is unused
The second line includes the number of toroidal slices, the number of R-Z
points on each slice, a symmetry option, and two unused values.

Following this are phi values (in degrees...) followed by R-Z values (in cm)
for all points on this divertor.

A second file is needed, although only one is needed for all divertors.  It
includes all points in the magnetic axis for one symmetry period, starting at
0 ending at the plane of symmetry and equidistantly spaced.  The points are
given in R-Z (although they are now in meters)

To calculate whether a point hits, the code draws a line between the axis and
each divertor section and records if it intersects.

--------outputs---------

There are two main types of outputs.  The first is a condensed output which
shows the starting and end location of each point along with a calculation of
connection length, and whether or not it terminated on a surface.

The connection length is calculated at the explicit step values, so too large
values will cause errors.

In addition to this, the code also prints out information for each point at
each explicit step.  This is currently the R, Z, phi, and mag(B).  Unless sent
to a file (preferred) this output is printed to screen.  This data can be used
for poincare plot generation and trajectory following.

In addition to the explicit steps, there is also an adaptive implicit step set
by the integrator (in this case dlsode).  All the calculations for field line
strikes are done at the implicit steps.

-------compiling and running--------

The code should be compilable with any compiler capable of handling fortran90.
A makefile is given, so a command of "make" should create the code.  The main
code is called "follow_to_wall" and can be executed from the command line
./follow_to_wall.  Currently, there are no command line options for the code.
To output the trajectory data to a file, use ./follow_to_wall > filename.

-------known issues-------

If a line turns around in phi the code will not be able to continue.  At this
point DLSODE will give an error, and it will stop following that point and
move to the next one.  This is currently the biggest issue with the code.


For more information contact Aaron Bader (abader@engr.wisc.edu) or Laurie
Stephey (stephey@wisc.edu)
