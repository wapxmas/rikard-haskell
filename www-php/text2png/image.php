<?php
header('Content-Type: image/png');

ini_set("display_errors", 1);
ini_set("track_errors", 1);
ini_set("html_errors", 1);
error_reporting(E_ALL);

require_once "GDText/Box.php";
require_once "GDText/Color.php";

use GDText\Box;
use GDText\Color;

$width = 490;
$height = 1000;

$imHeader = imagecreatetruecolor($width, $height);
$backgroundColor = imagecolorallocate($imHeader, 255, 255, 255);
imagefill($imHeader, 0, 0, $backgroundColor);
$boxHeader = new Box($imHeader);
$boxHeader->setFontFace(__DIR__.'/arial.ttf');
$boxHeader->setFontColor(new Color(0, 0, 0));
$boxHeader->setUnderlineColor(new Color(0xb0, 0xcc, 0x1f));
$boxHeader->setFontSize(14);
$boxHeader->setBox(5, 5, $width - 10, $height);
$boxHeader->setTextAlign('center', 'top');
$boxHeader->setLineHeight(1.6);
$textHeaderHeight = $boxHeader->draw($_GET["header"]) + 5;

$imText = imagecreatetruecolor($width, $height);
$backgroundColor = imagecolorallocate($imText , 255, 255, 255);
imagefill($imText , 0, 0, $backgroundColor);
$boxText = new Box($imText );
$boxText->setFontFace(__DIR__.'/arial.ttf');
$boxText->setFontColor(new Color(0, 0, 0));
$boxText->setUnderlineColor(new Color(255, 255, 255));
$boxText->setFontSize(14);
$boxText->setBox(5, 0, $width - 10, $height);
$boxText->setTextAlign('center', 'top');
$textTextHeight = $boxText->draw($_GET["text"]);

$new_height = $textTextHeight + $textHeaderHeight;

$imTotal = imagecreatetruecolor($width + 6, $new_height + 10 + 6);
$borderColor = imagecolorallocate($imHeader, 176, 204, 31);
imagefill($imTotal, 0, 0, $borderColor);

imagecopyresampled($imTotal, $imHeader, 3, 3, 0, 0, $width, $textHeaderHeight + 5, $width, $textHeaderHeight + 5);
imagecopyresampled($imTotal, $imText, 3, $textHeaderHeight + 5 + 3, 0, 0, $width, $textTextHeight + 5, $width, $textTextHeight + 5);

imagepng($imTotal);
imagedestroy($imTotal);
imagedestroy($imHeader);
