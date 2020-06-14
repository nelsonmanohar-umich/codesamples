# Write your MySQL query statement below

# Mary is a teacher in a middle school and she has a table seat storing students' names and their corresponding seat ids.
# 
# The column id is continuous increment.
# 
# 
# Mary wants to change seats for the adjacent students.
# 
# 
# Can you write a SQL query to output the result for Mary?
# 
# 
# +---------+---------+
# |    id   | student |
# +---------+---------+
# |    1    | Abbot   |
# |    2    | Doris   |
# |    3    | Emerson |
# |    4    | Green   |
# |    5    | Jeames  |
# +---------+---------+
# For the sample input, the output is:
# 
# 
# +---------+---------+
# |    id   | student |
# +---------+---------+
# |    1    | Doris   |
# |    2    | Abbot   |
# |    3    | Green   |
# |    4    | Emerson |
# |    5    | Jeames  |
# +---------+---------+
# Note:
# If the number of students is odd, there is no need to change the last one's seat.
# 
# 

select s.id,
(
case when 
     (select count(*)=s.id from seat)
then
    case
    when s.id mod 2
    then (select student from seat r where r.id=s.id)
    else (select student from seat r where r.id=s.id-1)
    end
else
    case 
    when id mod 2
    then (select student from seat r where r.id=s.id+1)
    else (select student from  seat r where r.id=s.id-1)
    end
end
) as student
from seat s
order by s.id;

