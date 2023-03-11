-module(skolan_utils).


-export([weighted_pup_per_teacher/2,
         average/1,
         bstring_to_float/1]).

weighted_pup_per_teacher(<<"-">>, _) ->
    <<"-">>;
weighted_pup_per_teacher(PupilsPerTeacher, <<"-">>)
  when is_binary(PupilsPerTeacher) ->
    weighted_pup_per_teacher(bstring_to_float(PupilsPerTeacher), 0.0);
weighted_pup_per_teacher(PupilsPerTeacher, CertifiedTeachersQuota) when
      is_binary(PupilsPerTeacher), is_binary(CertifiedTeachersQuota) ->
    weighted_pup_per_teacher(bstring_to_float(PupilsPerTeacher),
                             bstring_to_float(CertifiedTeachersQuota));
weighted_pup_per_teacher(PupilsPerTeacher, CertifiedTeachersQuota) when
      is_number(PupilsPerTeacher), is_number(CertifiedTeachersQuota) ->
    100*PupilsPerTeacher/(50.0 + CertifiedTeachersQuota*0.5);
weighted_pup_per_teacher(_, _) ->
    <<"-">>.

average(gy) -> weighted_pup_per_teacher(12.3, 83.5);
average(gr) -> weighted_pup_per_teacher(12.2, 71.0).

bstring_to_float(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N ++ ".0") of
        {error, no_float} ->
            {error, no_float};
        {Float, _} ->
            Float
    end.
