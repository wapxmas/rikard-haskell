{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module RikardCorp.Handlers.Main where

  import Yesod
  import RikardCorp.Foundation()
  import RikardCorp.Types as TPS
  import RikardCorp.Widgets
  import Data.List
  import qualified Data.Text as T

  sectionContentWidget :: SectionType -> UOkrug -> UPloshad -> UNaznach -> UPage -> Widget
  sectionContentWidget _sectionType _uOkrug _uPloshad _uNaznach _uPage =
    [whamlet|
    <div class="alert alert-info" role="alert">Объектов - нет.
    |]

  sectionHeaderWidget :: SectionType -> SectionData -> Widget
  sectionHeaderWidget st sd =
    [whamlet|
    <h1 class="hidden-xs text-center">#{title sd}
    <h1 class="visible-xs text-center text-lowercase">#{xsTitle sd}
    <nav class="row margin-top-20 hidden-print">
      <div class="col-md-4 col-sm-4 col-xs-6 hidden-xs">
        <span class="glyphicon glyphicon-eye-open glyph-color-green" aria-hidden="true">
        <a href="javascript:void(0)" onclick="document.location.href='@{MainR (usect st) uokrugNothing uploshadNothing unaznachNothing upageNothing}';">Показать все
      <div class="col-md-4 col-sm-4 col-xs-6 hidden-xs">
        <span class="glyphicon glyphicon-globe glyph-color-green" aria-hidden="true">
        <a href="#">Список на карте
      <div class="col-md-4 col-sm-4 col-xs-6 visible-xs text-center">
        <span class="glyphicon glyphicon-eye-open glyph-color-green" aria-hidden="true">
        <a href="javascript:void(0)" onclick="document.location.href='@{MainR (usect st) uokrugNothing uploshadNothing unaznachNothing upageNothing}';">Показать все
      <div class="col-md-4 col-sm-4 col-xs-6 visible-xs text-center">
        <span class="glyphicon glyphicon-globe glyph-color-green" aria-hidden="true">
        <a href="#">Список на карте
      <div class="col-md-4 col-sm-4 hidden-xs">
        <span class="glyphicon glyphicon-print glyph-color-green" aria-hidden="true">
        <a href="javascript:window.print()">Печать списка
    <hr>
    |]

  descriptionAddition :: UOkrug -> UPloshad -> UNaznach -> [T.Text]
  descriptionAddition (UOkrug okrug) (UPloshad ploshad) (UNaznach naznach) =
    fillElement okrug okrugToText ++
    fillElement naznach naznachToText ++
    fillElement ploshad ploshadToText
    where
      fillElement :: Maybe a -> (a -> T.Text) -> [T.Text]
      fillElement u f = maybe [] (flip (:) [] . f) u

  getMainR :: USection -> UOkrug -> UPloshad -> UNaznach -> UPage -> Handler Html
  getMainR (USection Nothing) _ _ _ _ = redirect IndexR
  getMainR uSection@(USection (Just secType)) uOkrug uPloshad uNaznach uPage = defaultLayout $ do
    let
      additions = descriptionAddition uOkrug uPloshad uNaznach
    setTitle . toHtml . T.unwords $ TPS.pageTitle (sectionData secType) : additions
    descWidget. T.intercalate "; " $ additions ++ [TPS.snippet (sectionData secType)]
    keyWidget . T.intercalate ", " $ additions ++ [TPS.keywords (sectionData secType)]
    mainTabsWidget
    let okruglist1 :: [Okrug]
        okruglist1 = take 6 okrugsList

        okruglist2 :: [Okrug]
        okruglist2 = okrugsList \\ okruglist1

        ploshadlist :: [Ploshad]
        ploshadlist = ploshadList

        naznachLglist1 :: [Naznachenie]
        naznachLglist1 = head naznachLgList

        naznachLglist2 :: [[Naznachenie]]
        naznachLglist2 = tail naznachLgList

        naznachMdlist :: [Naznachenie]
        naznachMdlist = head naznachMdList

        naznachXslist :: [Naznachenie]
        naznachXslist = head naznachXsList

        sData :: SectionData
        sData = sectionData secType
    [whamlet|
    <div class="row">
      <aside class="col-md-3 hidden-xs hidden-sm hidden-print">
        <div class="row">
          <section class="col-md-12">
            <h2>Округ
            <div class="row">
              <nav class="col-md-6">
                <ul class="list-unstyled list-padding-bottom">
                  $forall o <- okruglist1
                    <li :uokrug o == uOkrug:.selected-main-li>
                      <a href="@{MainR uSection (uokrug o) uPloshad uNaznach uPage}">#{okrugToText o}
              <nav class="col-md-6">
                <ul class="list-unstyled list-padding-bottom">
                  $forall o <- okruglist2
                    <li :uokrug o == uOkrug:.selected-main-li>
                      <a href="@{MainR uSection (uokrug o) uPloshad uNaznach uPage}">#{okrugToText o}
                  <li :uokrugNothing == uOkrug:.selected-main-li>
                    <a rel="nofollow" href="@{MainR uSection uokrugNothing uPloshad uNaznach uPage}">любой
        <div class="row">
          <section class="col-md-12">
            <h2>Площадь объекта
            <div class="row">
              <nav class="col-md-12">
                <ul class="list-unstyled list-padding-bottom">
                  $forall p <- ploshadlist
                    <li :uploshad p == uPloshad:.selected-main-li>
                      <a href="@{MainR uSection uOkrug (uploshad p) uNaznach uPage}">#{ploshadToText p}
                  <li :uploshadNothing == uPloshad:.selected-main-li>
                    <a rel="nofollow" href="@{MainR uSection uOkrug uploshadNothing uNaznach uPage}">любая
        <div class="row">
          <section class="col-md-12">
            <h2>Назначение объекта
            <div class="row">
              <nav class="col-md-12">
                <ul class="list-unstyled list-padding-bottom">
                  $forall n <- naznachLglist1
                    <li :unaznach n == uNaznach:.selected-main-li>
                      <a href="@{MainR uSection uOkrug uPloshad (unaznach n) uPage}">#{naznachToText n}
                  $forall ns <- naznachLglist2
                    <li>
                      <hr>
                    $forall n <- ns
                      <li :unaznach n == uNaznach:.selected-main-li>
                        <a href="@{MainR uSection uOkrug uPloshad (unaznach n) uPage}">#{naznachToText n}
                  <li :unaznachNothing == uNaznach:.selected-main-li>
                    <a rel="nofollow" href="@{MainR uSection uOkrug uPloshad unaznachNothing uPage}">любое
      <section class="col-md-9 col-sm-12 col-xs-12">
        ^{sectionHeaderWidget secType sData}
        <aside class="col-sm-12 col-xs-12 visible-xs visible-sm hidden-print">
          <div class="row">
            <section class="col-sm-12 col-xs-12">
              <h2 class="text-center">Округ
              <div class="row">
                <nav class="col-sm-12 col-xs-12">
                  <ul class="list-inline center-block text-center list-margin-bottom-top">
                    $forall o <- okrugsList
                      <li :uokrug o == uOkrug:.selected-main-li.selected-mini-li>
                        <a href="@{MainR uSection (uokrug o) uPloshad uNaznach uPage}">#{okrugToText o}
                    <li :uokrugNothing == uOkrug:.selected-main-li.selected-mini-li>
                      <a rel="nofollow" href="@{MainR uSection uokrugNothing uPloshad uNaznach uPage}">любой
        ^{sectionContentWidget secType uOkrug uPloshad uNaznach uPage}
    <aside class="row hidden-print">
      <div class="col-sm-12 col-xs-12 visible-xs visible-sm">
        <div class="row">
          <section class="col-sm-12 col-xs-12">
            <h2 class="text-center">Площадь объекта
            <div class="row">
              <nav class="col-sm-12 col-xs-12">
                <ul class="list-inline center-block text-center list-margin-bottom-top">
                  $forall p <- ploshadlist
                    <li :uploshad p == uPloshad:.selected-main-li.selected-mini-li>
                      <a href="@{MainR uSection uOkrug (uploshad p) uNaznach uPage}">#{ploshadToText p}
                  <li :uploshadNothing == uPloshad:.selected-main-li.selected-mini-li>
                    <a rel="nofollow" href="@{MainR uSection uOkrug uploshadNothing uNaznach uPage}">любая
        <div class="row">
          <section class="col-sm-12 col-xs-12">
            <h2 class="text-center">Назначение объекта
            <div class="row">
              <nav class="col-sm-12 col-xs-12">
                <ul class="visible-sm list-inline center-block text-center list-margin-bottom-top">
                  $forall n <- naznachMdlist
                    <li :unaznach n == uNaznach:.selected-main-li.selected-mini-li>
                      <a href="@{MainR uSection uOkrug uPloshad (unaznach n) uPage}">#{naznachToText n}
                <ul class="visible-xs list-inline center-block text-center list-margin-bottom-top">
                  $forall n <- naznachXslist
                    <li :unaznach n == uNaznach:.selected-main-li.selected-mini-li>
                      <a href="@{MainR uSection uOkrug uPloshad (unaznach n) uPage}">#{naznachToText n}
    |]
