{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module SDL.Pal.WASD where
import Linear.Extra
import Control.Monad.State hiding (get)
import Control.Lens.Extra
import SDL
import SDL.Pal.Window

moveSpeed :: Fractional a => a
moveSpeed = 0.01

applyMouseLook :: (Epsilon a, RealFloat a, Fractional a, MonadIO m, MonadState s m)
               => Window -> Lens' s (Pose a) -> m ()
applyMouseLook win poseLens = do
    (mode, V2 x y) <- getMouseLocationV2

    case mode of
        RelativeLocation -> do
            poseLens . posOrientation *= axisAngle (V3 0 1 0) (-x/500)
            poseLens . posOrientation *= axisAngle (V3 1 0 0) (-y/500)
        AbsoluteLocation -> do
            V2 w h <- get (windowSize win)
            let (x', y') = (x - (fromIntegral w / 2), y - (fromIntegral h / 2))
            poseLens . posOrientation .= axisAngle (V3 0 1 0) (-x'/500)
                                       * axisAngle (V3 1 0 0) (-y'/500)

-- | Move player by the given vector,
-- rotated to be relative to their current orientation
movePose :: (RealFloat a, Conjugate a, MonadState s m) => Lens' s (Pose a) -> V3 a -> m ()
movePose poseLens vec = do
    orient <- use $ poseLens . posOrientation
    poseLens . posPosition += rotate orient vec

turnPose :: (RealFloat a, MonadState s m) => Lens' s (Pose a) -> Quaternion a -> m ()
turnPose poseLens turn =
    -- Quat rotation must be rotation * original rather than vice versa
    poseLens . posOrientation %= (turn *)

applyWASD :: (RealFloat a, Num a, Conjugate a, MonadIO m, MonadState s m) => Lens' s (Pose a) -> m ()
applyWASD poseLens = do
    shiftDown <- isShiftDown
    let pos = moveSpeed    * if shiftDown then 10 else 1
        neg = (-moveSpeed) * if shiftDown then 10 else 1
    whenKeyPressed ScancodeW $ movePose poseLens (V3 0   0   neg)
    whenKeyPressed ScancodeS $ movePose poseLens (V3 0   0   pos)
    whenKeyPressed ScancodeA $ movePose poseLens (V3 neg 0   0  )
    whenKeyPressed ScancodeD $ movePose poseLens (V3 pos 0   0  )
    whenKeyPressed ScancodeQ $ movePose poseLens (V3 0   neg 0  )
    whenKeyPressed ScancodeE $ movePose poseLens (V3 0   pos 0  )

