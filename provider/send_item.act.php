<?php

/* *******************************************************
 *
 * 发送货币
 *
 * ***************************************************** */

class Act_Send_item extends Page
{

	public function __construct()
	{
		parent::__construct();
	}
	
    public function process()
    {
        if($this->input['submit'] == '发送')
        {
            $erl = new Erlang();
            $erl->connect();
            $arg = array(
                array(
                    $this->input['id_type'],
                    $this->input['id'],
                    (int)$this->input['tid'],
                    (int)$this->input['num'],
                )
            );
            $rt = $erl->get('lib_admin', 'add_item', '[~a, ~s, ~u, ~u]', $arg);
            if($rt[0] == "ok") {
                $this->assign('alert', "发送成功（{$rt[1]}：{$rt[2]}）");
            }else{
                $this->assign('alert', $rt[0]);
            }
        }
		$this->assign('id', $this->input['id']);
		$this->display();
	}

}
?>
