-define(_FN(), element(2, element(2, process_info(self(), current_function)))).
-define(DEBUG(_Msg), error_logger:info_report(lists:append([{file, ?FILE},
                                                            {function, ?_FN()},
                                                            {line, ?LINE}],
                                                           case is_list(_Msg) of true -> _Msg;
                                                                                 false -> [_Msg]
                                                           end))).
