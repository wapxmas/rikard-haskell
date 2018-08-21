{-# LANGUAGE TypeApplications #-}

module RikardCorp.Helpers.Geo (
module RikardCorp.Helpers.Geo,
GEO.Point(..), GEO.pt
) where

import qualified Data.List               as DL
import qualified Data.Map.Strict         as M
import           Data.Maybe
import qualified Data.Text               as T
import qualified Geo.Computations        as GEO
import qualified RikardCorp.Helpers.Json as HJ
import           RikardCorp.Types

getGeoAO :: [GeoAo] -> M.Map T.Text Okrug -> GEOPoint -> Maybe Okrug
getGeoAO [] _ _ = Nothing
getGeoAO (ao:aos) mao pt =
  let
    ps = HJ.decodeT' @GeoJsonGeometry . geoAoJson $ ao
  in
    if isNothing ps
      then Nothing
      else
        let
          isPip = isPointInPolygon (fromJust ps) pt
          okrug = M.lookup (geoAoAbbr ao) mao
          res
            | isPip = okrug
            | otherwise = getGeoAO aos mao pt
        in
          res

getGeoMO :: [GeoMo] -> M.Map T.Text Rayon -> GEOPoint -> Maybe Rayon
getGeoMO [] _ _ = Nothing
getGeoMO (mo:mos) mmo pt =
  let
    ps = HJ.decodeT' @GeoJsonGeometry . geoMoJson $ mo
  in
    if isNothing ps
      then Nothing
      else
        let
          isPip = isPointInPolygon (fromJust ps) pt
          rayon = M.lookup (geoMoName mo) mmo
          res
            | isPip = rayon
            | otherwise = getGeoMO mos mmo pt
        in
          res

isPointInPolygon :: GeoJsonGeometry -> GEOPoint -> Bool
isPointInPolygon (GeoJsonGeometryMultiPolygon _) _ = False
isPointInPolygon (GeoJsonGeometryPolygon (GeoJsonPolygon lrs)) pt =
  DL.any (isPointInPolygon' . isPointInPolygonPrep) lrs
  where
    isPointInPolygon' :: [GEOPoint] -> Bool
    isPointInPolygon' [] = False
    isPointInPolygon' ps =
      let
        p0 = head ps
        pm = last ps
        sm =
          if p0 == pm
            then isPointInPolygon'Res (init ps) p0
            else isPointInPolygon'Res ps p0
      in
        sum sm /= 0
      where
        isPointInPolygon'Calc :: GEOPoint -> GEOPoint -> Int
        isPointInPolygon'Calc p0 p1
          | GEO.pntLat p0 <= GEO.pntLat pt &&
            GEO.pntLat p1 > GEO.pntLat pt &&
            geoIsLeft p0 p1 pt > 0 = 1
          | GEO.pntLat p0 > GEO.pntLat pt &&
            GEO.pntLat p1 <= GEO.pntLat pt &&
            geoIsLeft p0 p1 pt < 0 = -1
          | otherwise = 0

        isPointInPolygon'Res :: [GEOPoint] -> GEOPoint -> [Int]
        isPointInPolygon'Res [] _ = []
        isPointInPolygon'Res pss lps =
          let
            (nxps, p0, p1) = isPointInPolygon'Get pss lps
          in
              isPointInPolygon'Calc p0 p1
            : isPointInPolygon'Res nxps lps

        isPointInPolygon'Get
          :: [GEOPoint] -> GEOPoint -> ([GEOPoint], GEOPoint, GEOPoint)
        isPointInPolygon'Get [] lps = ([], lps, lps)
        isPointInPolygon'Get (p0:p1:pss) _ = (p1:pss, p0, p1)
        isPointInPolygon'Get (p0:pss) lps = (pss, p0, lps)

        geoIsLeft :: GEOPoint -> GEOPoint -> GEOPoint -> Int
        geoIsLeft p0 p1 p2 =
          let
            c :: Double
            c = ((GEO.pntLon p1 - GEO.pntLon p0) * (GEO.pntLat p2 - GEO.pntLat p0)
               - (GEO.pntLon p2 - GEO.pntLon p0) * (GEO.pntLat p1 - GEO.pntLat p0))

            res :: Int
            res
              | c > 0 = 1
              | c < 0 = -1
              | otherwise = 0
          in
            res

    isPointInPolygonPrep :: GeoJsonLinearRing -> [GEOPoint]
    isPointInPolygonPrep (GeoJsonLinearRing pts) =
      fmap (\p ->
        let
          [pLon, pLat] = geoJsonPointCoords p
        in
          GEO.pt pLat pLon Nothing Nothing
        ) pts
