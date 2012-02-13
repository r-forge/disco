
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->
<!-- Project description is edited out -->
<!-- end of project description -->
<p> The <TT>disco</TT> package is not about music, but about <strong>dis</strong>tribution <strong>co</strong>mparison. This project contains the functions that accompany the book <a href=http://www.springer.com/statistics/book/978-0-387-92709-1>Comparing Distributions by Olivier Thas.</a>
</p>

<p>
<CENTER>
<a href='http://biomath.ugent.be/~othas/Comparing_Distributions/Home.html'>
<img src='http://biomath.ugent.be/~othas/Comparing_Distributions/Home_files/Cover3.jpg' alt='Book cover of Comparing Distributions' vspace=30/></a>
</CENTER>
</p>

<p>
  The package can be downloaded for Windows, Linux and Mac from <a href="https://r-forge.r-project.org/R/?group_id=1333"><strong>R-Forge</strong></a>, or installed using the command:   
  <TT>install.packages("disco", repos="http://R-Forge.R-project.org")</TT>
</p>

<p>
The original package was called <TT>cd</TT> and can still be downloaded from <a href=http://biomath.ugent.be/~othas/Comparing_Distributions/R-Package.html><strong>here</strong></a>. As this package is designed for R 2.10, it is time to update the code to the new standards and update the help files for the different functions. This update is currently in progress. While we're at it, the functions are generalized and refactored in order to increase performance and make the interfaces more intuitive. This effort will result in a version 2.0 of the <TT>disco</TT> package, formerly known as <TT>cd</TT>.
</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. If you have any questions regarding the project or the application of the functions, feel free to mail me at <a href="mailto:Joris.Meys@Ugent.be">Joris.Meys@Ugent.be</a></p>

</body>
</html>
