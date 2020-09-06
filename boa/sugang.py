# 설치된 패키지의 정보를 확인
# pip show <package-name>

# requests 라이브러리 (케네스 라이츠(Kenneth Reitz)가 개발)
# 복잡한 HTTP 요청과 쿠기, 헤더를 아주 잘 처리하며, 그 외에도 많은 기능이 있음
# pip install requests

# BeautifulSoup 패키지
# pip install beautifulsoup4

# selenium 패키지
# pip install selenium

# time 패키지
# pip install time

import requests
from bs4 import BeautifulSoup as bs
from selenium import webdriver
import time

session = requests.Session()

# 웹드라이버 실행하기
driver = webdriver.Chrome(executable_path="chromedriver.exe")

# 사이트에 접속하기
driver.get("https://sso.step.or.kr/member")

# 브라우저 창 최대화
driver.maximize_window()

# 사이트 접속하기 잠시 기다리기 (명시적 대기, 2초)
time.sleep(2)

# 소스코드에서 아이디(id), 비번(pw)의 파라미터 확인하고 입력하기
driver.find_element_by_id('id_input').send_keys('ekoreatech14@koreatech.ac.kr')
time.sleep(1)
driver.find_element_by_id('pw_input').send_keys('e-koreatech1')
time.sleep(1)

# 로그인 버튼을 찾아서 클릭하기 (로그인 버튼의 Xpath를 복사해서 가져오기)
driver.find_element_by_xpath('//*[@id="body"]/div/div/div/div[3]').click()
time.sleep(3)

# 로그인되었으나, e-koreatech 관리자 페이지가 아님, 관리지 페이지로 리다이렉트
# 아이디로 관리자 주소를 확인하고 입력해 줌
driver.get("https://sa.step.or.kr:11443/page/stepsa?m1=home%25")

# 홈 메뉴 클릭
driver.find_element_by_xpath('/html/body/div[2]/div[1]/div[3]/div[3]/div').click()

# 운영준비 > 기수개설관리 메뉴로 곧바로 이동
driver.get("https://sa.step.or.kr:11443/page/stepsa?m1=operation_manage%2Fterm%25")

# 필터정보 입력
# 게시판에서 필요한 데이터 크롤링 하기
# 또는 엑셀 파일 다운로드 하기'''




