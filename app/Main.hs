{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Data.Monoid
import Data.String
import Prelude hiding (id)

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!), Html)
import           Text.Blaze.Html5.Attributes hiding (item)
import           Text.Blaze.Html.Renderer.String

import Text.Hskribo.Quote

section = H.section
par     = H.p
ulist   = H.ul
item    = H.li


fragment :: Html
fragment = [hskribo|
(@section :id   "about-me-cmn"
          :lang "cmn"
  (par [我现在是一名大一学生，就读于加州大学圣迭戈分校。我的专业是语言学，计划第二专业数学、辅
    修政治科学。])
  (par [我的母语是 ,(mandarin) 英语是我的第二语言，但掌握不够熟练：词汇——尤其是生活词汇和生
    僻词——掌握较少，口语能力较差。我并不会其它语言。])
  (par [学术方面，我主要对计算语言学和程序语言理论比较感兴趣。具体到动机，我是 ,(strongAI)
    的支持者，希望提高机器对语言处理与理解的能力，并相信先进的编程语言是复杂的软件项目如人工智
    能的基石。我没有选择在本科就读计算机科学专业，有如下原因：])
  (@ulist
    (item [我已有多年编程经验 ,(csExp)，有算法竞赛的基础，可以自学计算机相关的知识。])
    (item [本校的学分限制。我本科四年最多只能学习六十门课，如果我选择计算机科学专业，我将失去
      学习数学、政治科学、或其它人文科学的机会。])
    (item [我较为懒散。为了 ,(nlp) 去学习机器学习，或者为了编程去学习 ,(archery) 这些「刚
      需」我还勉强能满足。可是，又有什么刚需让我去读古代经典或者高阶的数学呢？])))|]
  where
    wikiLink :: String -> Html
    wikiLink label = H.a ! href link $ fromString label
      where link = fromString $ "https://zh.wikipedia.org/wiki/" <> label

    mandarin = wikiLink "官话"
    strongAI = wikiLink "强人工智能"
    nlp      = wikiLink "自然语言处理"
    archery  = wikiLink "范畴论"
    csExp    = H.aside "我没有多少大项目的经验，更多的都是小玩具与原型。"

main :: IO ()
main = putStrLn $ renderHtml fragment
